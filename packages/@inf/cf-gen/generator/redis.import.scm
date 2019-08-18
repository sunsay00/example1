(module redis (redis-emit)
  (import scheme chicken data-structures tools signatures matchable migrations)
  (require-extension srfi-13 srfi-1)

  (define (pluralize name)
    (match name
           ('sub 'subs)
           (_ (error "undefined puralized case '" name))))

  (define (gen-expire key method)
    (if (method-cache-expire? method)
      (list "multi.expire(`" key "`, " (method->cache-expire method) ");")
      (list "multi.expire(`" key "`, 60);")))

  (define (gen-select-one model method ufield)
      (list
        "\n    const cuid = await this._client.hget(`" (model->name model) "." ufield "`, " ufield ");"
        "\n    if (cuid != undefined) {"
        "\n      return await this." (model->name model) "FindById($ctx, cuid);"
        "\n    } else {"
        "\n      const ret = await this._store." (store-call-emit model method) ";"
        "\n      if (ret != undefined) {"
        "\n        const multi = this._client.multi();"
        "\n        multi.hset(`" (model->name model) "." ufield "`, " ufield ", ret.id);"
        "\n        " (gen-expire (list (model->name model) "." ufield) method)
        "\n        multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
        "\n        multi.zadd(`" (model->name model) "`, [[ret.id, JSON.stringify(ret)]]);"
        "\n        " (gen-expire (list (model->name model)) method)
        "\n        await multi.exec();"
        "\n      }"
        "\n      return ret;"
        "\n    }"))

  (define (gen-select-non-ref-many model method)
    (let ((key (cached-array-key model method)))
      (list
        "\n    const ret = await this._client.hget(`" key "`, JSON.stringify([after, count]));"
        "\n    if (ret) {"
        "\n      return JSON.parse(ret);"
        "\n    } else {"
        "\n      const ret = await this._store." (store-call-emit model method) ";"
        "\n      const multi = this._client.multi();"
        "\n      multi.hset(`" key "`, JSON.stringify([after, count]), JSON.stringify(ret));"
        "\n      " (gen-expire key method)
        "\n      await multi.exec();"
        "\n      return ret;"
        "\n    }")))

  (define (gen-select-in model method x xs)
    (list
      "\n    let $" xs " = _.uniq(" xs ").sort();"
      "\n    if (after != undefined) { const $after = after; $" xs " = $" xs ".filter(" x " => " x " > $after); }"
      "\n    if (count != undefined) { $" xs ".length = Math.min($" xs ".length, count); }"
      ;"\n    const multi = this._client.multi();"
      "\n    if ($" xs ".length == 0) return [];"
      "\n    const $cuids = await this._client.hmget('" (model->name model) "." xs "', $" xs ");"
      "\n    if ($cuids.length != " xs ".length) throw new Error('cache length mismatch for " xs "');"
      "\n    const $missing = " xs ".reduce((sum, ids, i) => $cuids[i] != undefined ? sum : [...sum, ...ids.split(' ')], [] as string[]);"
      "\n    if ($missing.length == 0) {"
      "\n      return await this." (model->name model) "FindByIdIn($ctx, ($cuids as string[]).reduce((sum, i) => [...sum, ...i.split(' ')], [] as string[]), after, count);"
      "\n    } else {"
      "\n      const ret = await this._store." (store-call-emit model (method-param-rename method xs (string->symbol (string-append "$" (symbol->string xs))))) ";"
      "\n      if (ret.length > 0) {"
      "\n        const multi = this._client.multi();"
      "\n        multi.hmset(`" (model->name model) "." xs "`, ret.map<[string, string]>(r => [r." x ", ret.filter(i => i." x " == r." x ").map(i => i.id).join(' ')]));"
      "\n        " (gen-expire (list (model->name model) "." xs) method)
      "\n        ret.map(r => multi.zremrangebyscore(`" (model->name model) "`, r.id, r.id));"
      "\n        multi.zadd(`" (model->name model) "`, ret.map<[string, string]>(r => [r.id, JSON.stringify(r)]));"
      "\n        " (gen-expire (list (model->name model)) method)
      "\n        await multi.exec();"
      "\n      }"
      "\n      return ret;"
      "\n    }"
      ))

  (define (gen-select-many model method)
    (let ((key (cached-array-key model method)))
      (list
        "\n    if (await this._client.hget(`" key ".exists`, JSON.stringify([after, count]))) {"
        (if (method-ascending? method)
            (list "\n      const cret = await this._client.zrangebyscore(`" key "`, after == undefined ? '-inf' : after, '+inf', count == undefined ? undefined : {offset: 0, count});")
            (list "\n      const cret = await this._client.zrevrangebyscore(`" key "`, after == undefined ? '+inf' : after, '-inf', count == undefined ? undefined : {offset: 0, count});"))
        "\n      return cret.map(r => JSON.parse(r));"
        "\n    } else {"
        "\n      const ret = await this._store." (store-call-emit model method) ";"
        "\n      const multi = this._client.multi();"
        "\n      multi.hset(`" key ".exists`, JSON.stringify([after, count]), JSON.stringify(true));"
        "\n      " (gen-expire (list key ".exists") method)
        "\n      ret.map(r => multi.zremrangebyscore(`" key "`, r.id, r.id));"
        "\n      multi.zadd(`" key "`, ret.map<[string, string]>(r => [r.id, JSON.stringify(r)]));"
        "\n      " (gen-expire (list key) method)
        "\n      await multi.exec();"
        "\n      return ret;"
        "\n    }")))

  (define (query-command model method desc? cmd)
    (let ((ufields (model->unique-index-fieldnames model)))
      (match cmd
             (`(command (select (distinct ,x))) (gen-select-non-ref-many model method))
             (`(command (select (distinct ,x)) (where ,w)) (gen-select-non-ref-many model method))
             (`(command (select . ,ss) (where (= parentId 'parentId)))
               (let ((model-name (model->name model)))
                 (list
                   "\n    const cret = await this._client.hget('" model-name ".parents', parentId);"
                   "\n    if (cret != undefined) {"
                   "\n      return this." model-name "FindById($ctx, cret);"
                   "\n    } else {"
                   "\n      const ret = await this._store." model-name "FindByParentId($ctx, parentId);"
                   "\n      if (ret != undefined) {"
                   "\n        const multi = this._client.multi();"
                   "\n        multi.zremrangebyscore('" model-name "', ret.id, ret.id);"
                   "\n        multi.zadd('" model-name "', [[ret.id, JSON.stringify(ret)]]);"
                   "\n        " (gen-expire (list model-name) method)
                   "\n        multi.hset('" model-name ".parents', parentId, ret.id);"
                   "\n        " (gen-expire (list model-name ".exists") method)
                   "\n        await multi.exec();"
                   "\n      }"
                   "\n      return ret;"
                   "\n    }"
                   )))
             (`(command (select . ,ss) (where (in id 'ids)))
               (list 
                 "\n    let ids2 = _.uniq(ids).sort();"
                 "\n    if (after != undefined) { const after2 = after; ids2 = ids2.filter(id => id > after2); }"
                 "\n    if (count != undefined) { ids2.length = count; }"
                 "\n    const multi = this._client.multi();"
                 "\n    ids2.forEach(id => multi.zrangebyscore(`" (model->name model) "`, id, id));"
                 "\n    const result = await multi.exec();"
                 "\n    const missingIds = ids2.filter((id, i) => !result[i]);"
                 "\n    if (missingIds.length == 0) {"
                 "\n      return result.map(r => JSON.parse(r));"
                 "\n    } else {"
                 "\n      const ret = await this._store." (store-call-emit model (method-param-rename method 'ids 'ids2)) ";"
                 "\n      if (ret.length > 0) {"
                 "\n        const multi = this._client.multi();"
                 "\n        ret.map(r => multi.zremrangebyscore(`" (model->name model) "`, r.id, r.id));"
                 "\n        multi.zadd(`" (model->name model) "`, ret.map<[string, string]>(r => [r.id, JSON.stringify(r)]));"
                 "\n        " (gen-expire (list (model->name model)) method)
                 "\n        await multi.exec();"
                 "\n      }"
                 "\n      return ret;"
                 "\n    }"))
             (`(command (select . ,?ss) (where (and (in ,?make ',?makes) (= (lower ,?exteriorColor) (lower ',?exteriorColor)) (= (lower ,?model) (lower ',?model)))))
               (list 
                 "\n    // TODO: cache search"
                 "\n    return this._store." (store-call-emit model method) ";"))
             (`(command (select . ,ss) (where (in sub 'subs)))
               (list
                 "\n    let subs2  = _.uniq(subs).sort();"
                 "\n    if (after != undefined) { const after2 = after; subs2 = subs2.filter(sub => sub > after2); }"
                 "\n    if (count != undefined) { subs2.length = count; }"
                 ;"\n    const multi = this._client.multi();"
                 "\n    if (subs.length == 0) return [];"
                 "\n    const cuids = await this._client.hmget('" (model->name model) ".subs', subs2);"
                 "\n    if (cuids.length != subs.length) throw new Error('cache length mismatch');"
                 "\n    const missingSubs = subs.filter((id, i) => cuids[i] == undefined);"
                 "\n    if (missingSubs.length == 0) {"
                 "\n      return await this." (model->name model) "FindByIdIn($ctx, cuids as string[], after, count);"
                 "\n    } else {"
                 "\n      const ret = await this._store." (store-call-emit model (method-param-rename method 'subs 'subs2)) ";"
                 "\n      if (ret.length > 0) {"
                 "\n        const multi = this._client.multi();"
                 "\n        multi.hmset(`" (model->name model) ".subs`, ret.map<[string, string]>(r => [r.sub, r.id]));"
                 "\n        " (gen-expire (list (model->name model) ".subs") method)
                 "\n        ret.map(r => multi.zremrangebyscore(`" (model->name model) "`, r.id, r.id));"
                 "\n        multi.zadd(`" (model->name model) "`, ret.map<[string, string]>(r => [r.id, JSON.stringify(r)]));"
                 "\n        " (gen-expire (list (model->name model)) method)
                 "\n        await multi.exec();"
                 "\n      }"
                 "\n      return ret;"
                 "\n    }"
                 ))
             (`(command (select . ,ss) (where (in ,x ',xs))) (gen-select-in model method x xs))
             (`(command (select . ,ss) (where (and (in ,x ',xs) (= ,a1 ,a2)))) (gen-select-in model method x xs))
             ((or `(command (select . ,ss) (where (= id 'id)))
                  `(command (select . ,ss) (where (and (= id 'id) (= sub $sub)))))
              (list
                "\n    const cret = await this._client.zrangebyscore('" (model->name model) "', id, id);"
                "\n    if (cret.length > 0) {"
                (if (sub-field? model method)
                    (list "\n      const ret = JSON.parse(cret[0]);"
                          "\n      return ret.sub == $ctx.sub ? ret : undefined;")
                    (list "\n      return JSON.parse(cret[0]);"))
                "\n    } else {"
                "\n      const ret = await this._store." (store-call-emit model method) ";"
                "\n      if (ret != undefined) {"
                "\n        const multi = this._client.multi();"
                "\n        multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                "\n        multi.zadd(`" (model->name model) "`, [[ret.id, JSON.stringify(ret)]]);"
                "\n        " (gen-expire (list (model->name model)) method)
                "\n        await multi.exec();"
                "\n      }"
                "\n      return ret;"
                "\n    }"))
             (`(command (select . ,ss) (where (= sub $sub)))
               (let ((key (cached-array-key model method)))
                 (list
                   "\n    if (await this._client.hget(`" key ".exists`, JSON.stringify([after, count]))) {"
                   (if (method-ascending? method)
                       (list "\n      const cret = await this._client.zrangebyscore(`" key "`, after == undefined ? '-inf' : after, '+inf', count == undefined ? undefined : {offset: 0, count});")
                       (list "\n      const cret = await this._client.zrevrangebyscore(`" key "`, after == undefined ? '+inf' : after, '-inf', count == undefined ? undefined : {offset: 0, count});"))
                   "\n      return cret.map(r => JSON.parse(r));"
                   "\n    } else {"
                   "\n      const ret = await this._store." (store-call-emit model method) ";"
                   "\n      const multi = this._client.multi();"
                   "\n      multi.hset(`" key ".exists`, JSON.stringify([after, count]), JSON.stringify(true));"
                   "\n      " (gen-expire (list key ".exists") method)
                   "\n      ret.map(r => multi.zremrangebyscore(`" key "`, r.id, r.id));"
                   "\n      multi.zadd(`" key "`, ret.map<[string, string]>(r => [r.id, JSON.stringify(r)]));"
                   "\n      " (gen-expire (list key) method)
                   "\n      await multi.exec();"
                   "\n      return ret;"
                   "\n    }")))
             (`(command (select . ,ss) (where (= sub ',var)))
               (list
                 "\n    const cuid = await this._client.hget(`" (model->name model) "." (pluralize var) "`, " var ");"
                 "\n    if (cuid != undefined) {"
                 "\n      return await this." (model->name model) "FindById($ctx, cuid);"
                 "\n    } else {"
                 "\n      const ret = await this._store." (store-call-emit model method) ";"
                 "\n      if (ret != undefined) {"
                 "\n        const multi = this._client.multi();"
                 "\n        multi.hset(`" (model->name model) "." (pluralize var) "`, " var ", ret.id);"
                 "\n        " (gen-expire (list (model->name model) "." (pluralize var)) method)
                 "\n        multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                 "\n        multi.zadd(`" (model->name model) "`, [[ret.id, JSON.stringify(ret)]]);"
                 "\n        " (gen-expire (list (model->name model)) method)
                 "\n        await multi.exec();"
                 "\n      }"
                 "\n      return ret;"
                 "\n    }"))
             (`(command (select . ,ss) (where ,w) (groupby ,m))
               (list
                 "\n    return this._store." (store-call-emit model method) ";"))
             (`(command (select . ,ss) (where (and (<= (distanceSphere ,location ',location) . ,?) . ,??)))
               (list
                 "\n    // TODO: cache geo"
                 "\n    return this._store." (store-call-emit model method) ";"))
             (`(command (select . ,ss) (where (<= (distanceSphere ,location ',location)  . ,?)))
               (list
                 "\n    // TODO: cache geo"
                 "\n    return this._store." (store-call-emit model method) ";"))
             (`(command (select . ,ss) (where (and (= (lower ,field1) (lower ',var1)) (= (lower ,field2) (lower ',var2)))))
               (list
                 "\n    // TODO: " field1 " - " field2
                 "\n    return this._store." (store-call-emit model method) ";"))
             (`(command (select . ,ss) (where (= ,?field ',?var)))
               ;(if (member ?field ufields)
               (if (and (pair? ufields) (eq? ?field (car ufields)))
                   (gen-select-one model method ?field)
                   (gen-select-many model method)))
             (`(command (select . ,ss) (groupby . ,g)) (gen-select-many model method))
             (`(command (select . ,ss) (orderby . ,os)) (gen-select-many model method))
             (`(command (select . ,ss) (where ,w) (orderby . ,os)) (gen-select-many model method))
             (`(command (select . ,ss)) (gen-select-many model method))
             (`(command (update (= ,elements (push ,element))) (where (= id 'id)))
               (let ((model-name (model->name model)))

                 `("\n    const ret = await this._store." ,model-name "Append" ,(first-up element) "($ctx, id, " ,element ");"
                   "\n    if (ret != undefined) await this._client.zremrangebyscore('" ,model-name "', id, id);"
                   "\n    return ret;")))
             (`(command (update (= (at ,elements ',elementIndex) ,element)) (where (= id 'id)))
               (let ((model-name (model->name model)))
                 `("\n   const ret = await this._store." ,model-name "Update" ,(first-up element) "($ctx, id, " ,elementIndex ", " ,element ");"
                   "\n   if (ret != undefined) await this._client.zremrangebyscore('" ,model-name "', id, id);"
                   "\n   return ret;")))
             (`(command (update . ,ss) (where ,w) (return . ,rs))
               (let ((array-keys (cached-array-keys model)))
                 (list
                   "\n    const ret = await this._store." (store-call-emit model method) ";"
                   "\n    if (ret != undefined) {"
                   "\n      const multi = this._client.multi();"
                   (if (null? array-keys)
                       (list
                         "\n      this._client.zremrangebyscore('" (model->name model) "', ret.id, ret.id);"
                         (map (lambda (ufield) (list "\n      multi.hdel(`" (model->name model) "." ufield "`, [ret." ufield "]);")) ufields))
                       (list
                         "\n      multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                         "\n      multi.del([" (intersperse (map (lambda (key) (list "`" key "`")) array-keys) ", ") "]);"
                         "\n      multi.del([" (intersperse (map (lambda (key) (list "`" key ".exists`")) array-keys) ", ") "]);"))
                   "\n      await multi.exec();"
                   "\n    }"
                   "\n    return ret;")))
             (`(command (insert . ,vs) (return . ,rs))
               (let ((array-keys (cached-array-keys model)))
                 (list
                   "\n    const ret = await this._store." (store-call-emit model method) ";"
                   (if (null? array-keys) 
                       (list
                         "\n    const multi = this._client.multi();"
                         "\n    multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                         "\n    multi.zadd(`" (model->name model) "`, [[ret.id, JSON.stringify(ret)]]);"
                         "\n    " (gen-expire (list (model->name model)) method)
                         "\n    await multi.exec();")
                       (list
                         "\n    const multi = this._client.multi();"
                         "\n    multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                         "\n    multi.zadd(`" (model->name model) "`, [[ret.id, JSON.stringify(ret)]]);"
                         "\n    " (gen-expire (list (model->name model)) method)
                         "\n    multi.del([" (intersperse (map (lambda (key) (list "`" key "`")) array-keys) ", ") "]);"
                         "\n    multi.del([" (intersperse (map (lambda (key) (list "`" key ".exists`")) array-keys) ", ") "]);"
                         "\n    await multi.exec();"))
                   "\n    return ret;")))
             (`(command (insert . ,vs) (on . ,os) (return . ,rs))
               (let ((array-keys (cached-array-keys model)))
                 (list
                   "\n    const ret = await this._store." (store-call-emit model method) ";"
                   (if (null? array-keys) 
                       (list
                         "\n    const multi = this._client.multi();"
                         "\n    multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                         "\n    multi.zadd(`" (model->name model) "`, [[ret.id, JSON.stringify(ret)]]);"
                         "\n    " (gen-expire (list (model->name model)) method)
                         "\n    await multi.exec();")
                       (list
                         "\n    const multi = this._client.multi();"
                         "\n    multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                         "\n    multi.zadd(`" (model->name model) "`, [[ret.id, JSON.stringify(ret)]]);"
                         "\n    " (gen-expire (list (model->name model)) method)
                         "\n    multi.del([" (intersperse (map (lambda (key) (list "`" key "`")) array-keys) ", ") "]);"
                         "\n    multi.del([" (intersperse (map (lambda (key) (list "`" key ".exists`")) array-keys) ", ") "]);"
                         "\n    await multi.exec();"))
                   "\n    return ret;")))
             (`(command (insert . ,vs) (on . ,os) (update . ,us) (return . ,rs))
               (let ((array-keys (cached-array-keys model)))
                 (list
                   "\n    const ret = await this._store." (store-call-emit model method) ";"
                   "\n    const multi = this._client.multi();"
                   (if (null? array-keys) 
                       (list
                         "\n    multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                         "\n    multi.zadd(`" (model->name model) "`, [[ret.id, JSON.stringify(ret)]]);"
                         "\n    " (gen-expire (list (model->name model)) method)
                         (map (lambda (ufield) (list "\n      multi.hdel(`" (model->name model) "." ufield "`, [ret." ufield "]);")) ufields))
                       (list
                         "\n    multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                         "\n    multi.zadd(`" (model->name model) "`, [[ret.id, JSON.stringify(ret)]]);"
                         "\n    " (gen-expire (list (model->name model)) method)
                         "\n    multi.del([" (intersperse (map (lambda (key) (list "`" key "`")) array-keys) ", ") "]);"
                         "\n    multi.del([" (intersperse (map (lambda (key) (list "`" key ".exists`")) array-keys) ", ") "]);"))
                   "\n    await multi.exec();"
                   "\n    return ret;")))
             (`(command (delete) (where ,w) (return . ,rs))
               (let ((array-keys (cached-array-keys model)))
                 (list
                   "\n    const ret = await this._store." (store-call-emit model method) ";"
                   "\n    if (ret != undefined) {"
                   "\n      const multi = this._client.multi();"
                   (if (null? array-keys)
                       (if (sub-field? model method)
                           (list "\n      multi.hdel(`" (model->name model) ".subs`, [ret.sub]);"
                                 "\n      multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);")
                           (list "\n      multi.zremrangebyscore('" (model->name model) "', ret.id, ret.id);"
                                 (map (lambda (ufield) (list "\n      multi.hdel(`" (model->name model) "." ufield "`, [ret." ufield "]);")) ufields)))
                       (list "\n      multi.zremrangebyscore(`" (model->name model) "`, ret.id, ret.id);"
                         (if (not (sub-field? model method)) ""
                             (list "\n      multi.hdel(`" (model->name model) ".subs`, [ret.sub]);"))
                         "\n      multi.del([" (intersperse (map (lambda (key) (list "`" key "`")) array-keys) ", ") "]);"
                         "\n      multi.del([" (intersperse (map (lambda (key) (list "`" key ".exists`")) array-keys) ", ") "]);"))
                   "\n      await multi.exec();"
                   "\n    }"
                   "\n    return ret;")))
             (else (error "(redis.import) failed to generate cache code from" cmd)))))

  (define (redis-emit api model method ascending? cmd)
    (let ((desc? (not ascending?))
          (model-name (model->name model)))
      (query-command model method desc? cmd)))
  )
