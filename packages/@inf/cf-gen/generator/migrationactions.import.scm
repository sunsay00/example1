(module migrationactions (do-action undo-action)

  (import scheme chicken utils data-structures tools signatures)
  (require-extension srfi-13 srfi-1)

  (define actions
    `((user 
        ,(lambda ()
           (list
             "\nDO"
             "\n$do$"
             "\nBEGIN"
             "\n  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname='api') THEN"
             "\n    CREATE USER api WITH PASSWORD 'changeme'; ALTER USER api VALID UNTIL 'infinity';"
             "\n  END IF;"
             "\nEND"
             "\n$do$;"
             "\nGRANT ALL ON schema public TO api;"
             "\nGRANT CONNECT ON DATABASE postgres TO api;"
             "\nGRANT SELECT ON TABLE serviceId TO api;"
             "\nGRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO api;"
             "\nGRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO api;"
             "\nALTER DEFAULT PRIVILEGES FOR ROLE root IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO api;"))
        ,(lambda ()
           (list
             "\nALTER DEFAULT PRIVILEGES FOR ROLE root IN SCHEMA public REVOKE SELECT, INSERT, UPDATE, DELETE ON TABLES FROM api;"
             "\nREVOKE USAGE ON ALL SEQUENCES IN SCHEMA public FROM api;"
             "\nREVOKE SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public FROM api;"
             "\nREVOKE SELECT ON TABLE serviceId FROM api;"
             "\nREVOKE CONNECT ON DATABASE postgres FROM api;"
             "\nREVOKE ALL ON schema public FROM api;"
             "\nDROP USER api;")))
      (gen-id-fn
        ,(lambda ()
           (list
             "\nCREATE FUNCTION generate_id(OUT result BIGINT) AS $$"
             "\n  DECLARE"
             "\n    -- TO START IDS SMALLER, YOU COULD CHANGE THIS TO A MORE RECENT UNIX TIMESTAMP"
             "\n    our_epoch bigint := 1483228800;"
             "\n    seq_id bigint;"
             "\n    now_millis bigint;"
             "\n    service_id int;"
             "\n  BEGIN"
             "\n      -- UNIQUE SERVICE IDENTIFIER CHANGE THIS FOR EACH SERVICE!!!"
             "\n      SELECT id FROM serviceId INTO service_id;"
             "\n      SELECT nextval('global_id_sequence') % 1024 INTO seq_id;"
             "\n      SELECT FLOOR(EXTRACT(EPOCH FROM clock_timestamp())) INTO now_millis;"
             "\n      result := (now_millis - our_epoch) << 20;"
             "\n      result := result | (service_id << 10);"
             "\n      result := result | (seq_id);"
             "\n  END;"
             "\n$$ LANGUAGE PLPGSQL;"))
        ,(lambda ()
           (list
             "\nDROP FUNCTION generate_id(OUT result BIGINT)")))
      ))

  (define (do-action name)
    (let ((result (assq name actions)))
      (if (not result) ""
        (let ((doit (cadr result)))
              (lambda rest 
                (smoosh (apply doit rest)))))))

  (define (undo-action name)
    (let ((result (assq name actions)))
      (if (not result) ""
        (let ((undoit (caddr result)))
              (lambda rest 
                (smoosh (apply undoit rest)))))))

)

