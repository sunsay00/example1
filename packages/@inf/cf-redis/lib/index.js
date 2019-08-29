var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __spreadArrays = (this && this.__spreadArrays) || function () {
    for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
    for (var r = Array(s), k = 0, i = 0; i < il; i++)
        for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
            r[k] = a[j];
    return r;
};
Object.defineProperty(exports, "__esModule", { value: true });
var Redis = require("redis");
var CacheClient = /** @class */ (function () {
    function CacheClient(redisUrl) {
        var _this = this;
        this.init = function (options) { return __awaiter(_this, void 0, void 0, function () {
            var client, err_1;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        client = Redis.createClient({
                            url: this._redisUrl,
                            connect_timeout: 5000,
                        });
                        _a.label = 1;
                    case 1:
                        _a.trys.push([1, 3, , 4]);
                        //console.log('REDIS CLIENT CREATED...');
                        return [4 /*yield*/, new Promise(function (resolve, reject) {
                                client.on('end', function (err) { return err ? resolve(err) : reject(); });
                                client.on('ready', function () { resolve(); });
                            })];
                    case 2:
                        //console.log('REDIS CLIENT CREATED...');
                        _a.sent();
                        //console.log('REDIS CONNECTION ESTABILISHED');
                        this._client = client;
                        return [3 /*break*/, 4];
                    case 3:
                        err_1 = _a.sent();
                        //console.error(`FAILED TO CONNECT TO REDIS - ${JSON.stringify(err)}`);
                        this._client = undefined;
                        throw err_1;
                    case 4: return [2 /*return*/];
                }
            });
        }); };
        this.uninit = function () { return __awaiter(_this, void 0, void 0, function () {
            var client_1;
            return __generator(this, function (_a) {
                if (this._client != undefined) {
                    client_1 = this._client;
                    this._client = undefined;
                    return [2 /*return*/, new Promise(function (resolve, reject) { return client_1.quit(function (err) { return err ? reject(err) : resolve(); }); })];
                }
                return [2 /*return*/];
            });
        }); };
        this.reset = function () { return __awaiter(_this, void 0, void 0, function () {
            var client;
            return __generator(this, function (_a) {
                if (process.env.NODE_ENV != 'test') {
                    throw new Error('ERROR!! cache reset should only be called in the test environment! ignoring');
                }
                if (this._client === undefined)
                    throw new Error('redis client not initialized');
                client = this._client;
                return [2 /*return*/, new Promise(function (resolve, reject) {
                        return client.flushall(function (err) { return err ? reject(err) : resolve(); });
                    })];
            });
        }); };
        this.ensureClient = function () { return __awaiter(_this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (!(this._client == undefined)) return [3 /*break*/, 2];
                        return [4 /*yield*/, this.init()];
                    case 1:
                        _a.sent();
                        if (this._client == undefined) {
                            throw new Error('redis client not initialized');
                        }
                        _a.label = 2;
                    case 2: return [2 /*return*/, this._client];
                }
            });
        }); };
        this._redisUrl = redisUrl;
    }
    //delete by prefix
    //client.EVAL(`return redis.call('del', 'defaultKey', unpack(redis.call('keys', ARGV[1])))`, 0, 'prefix:*');
    CacheClient.prototype.set = function (key, value) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_2;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 3, , 5]);
                        return [2 /*return*/, new Promise(function (resolve, reject) { return client.set(key, value, function (err) { return err ? reject(err) : resolve(); }); })];
                    case 3:
                        err_2 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 4:
                        _a.sent();
                        throw err_2;
                    case 5: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.get = function (key) {
        return __awaiter(this, void 0, void 0, function () {
            var client, ret, err_3;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.get(key, function (err, res) { return err ? reject(err) : resolve(res); }); })];
                    case 3:
                        ret = _a.sent();
                        return [2 /*return*/, ret === null ? undefined : ret];
                    case 4:
                        err_3 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_3;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.del = function (keys) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_4;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.DEL(keys, function (err) { return err ? reject(err) : resolve(); }); })];
                    case 3:
                        _a.sent();
                        return [3 /*break*/, 6];
                    case 4:
                        err_4 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_4;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.expire = function (key, seconds) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_5;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.EXPIRE(key, seconds, function (err) { return err ? reject(err) : resolve(); }); })];
                    case 3:
                        _a.sent();
                        return [3 /*break*/, 6];
                    case 4:
                        err_5 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_5;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.exists = function (key) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_6;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.EXISTS(key, function (err, res) { return err ? reject(err) : resolve(res != 0); }); })];
                    case 3: return [2 /*return*/, _a.sent()];
                    case 4:
                        err_6 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_6;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.zadd = function (key, values) {
        return __awaiter(this, void 0, void 0, function () {
            var params, client, err_7;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        //console.info(`REDIS ZADD: ${key}`);
                        if (values.length == 0)
                            return [2 /*return*/];
                        params = values.reduce(function (sum, i) { return __spreadArrays(sum, i); }, []);
                        return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.ZADD(key, params, function (err) { return err ? reject(err) : resolve(); }); })];
                    case 3:
                        _a.sent();
                        return [3 /*break*/, 6];
                    case 4:
                        err_7 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_7;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.hget = function (key, field) {
        return __awaiter(this, void 0, void 0, function () {
            var client, ret, err_8;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.HGET(key, field, function (err, res) { return err ? reject(err) : resolve(res); }); })];
                    case 3:
                        ret = _a.sent();
                        return [2 /*return*/, ret === null ? undefined : ret];
                    case 4:
                        err_8 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_8;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.hmget = function (key, fields) {
        return __awaiter(this, void 0, void 0, function () {
            var client, ret, err_9;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.HMGET(key, fields, function (err, res) { return err ? reject(err) : resolve(res); }); })];
                    case 3:
                        ret = _a.sent();
                        return [2 /*return*/, ret.map(function (r) { return r === null ? undefined : r; })];
                    case 4:
                        err_9 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_9;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.hset = function (key, field, value) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_10;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.HSET(key, field, value, function (err) { return err ? reject(err) : resolve(); }); })];
                    case 3:
                        _a.sent();
                        return [3 /*break*/, 6];
                    case 4:
                        err_10 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_10;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.hmset = function (key, values) {
        return __awaiter(this, void 0, void 0, function () {
            var params, client, err_11;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        params = values.reduce(function (sum, i) { return __spreadArrays(sum, i); }, []);
                        return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.HMSET(key, params, function (err) { return err ? reject(err) : resolve(); }); })];
                    case 3:
                        _a.sent();
                        return [3 /*break*/, 6];
                    case 4:
                        err_11 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_11;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.hdel = function (key, fields) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_12;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.HDEL(key, fields, function (err) { return err ? reject(err) : resolve(); }); })];
                    case 3:
                        _a.sent();
                        return [3 /*break*/, 6];
                    case 4:
                        err_12 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_12;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.zremrangebyscore = function (key, minScore, inclusiveMaxScore) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_13;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) {
                                return client.ZREMRANGEBYSCORE(key, minScore, inclusiveMaxScore, function (err) { return err ? reject(err) : resolve(); });
                            })];
                    case 3:
                        _a.sent();
                        return [3 /*break*/, 6];
                    case 4:
                        err_13 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_13;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.zrangebyscore = function (key, inclusiveMinScore, inclusiveMaxScore, options) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_14;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 7, , 9]);
                        if (!(options == undefined)) return [3 /*break*/, 4];
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.ZRANGEBYSCORE(key, inclusiveMinScore, inclusiveMaxScore, function (err, items) { return err ? reject(err) : resolve(items); }); })];
                    case 3: return [2 /*return*/, _a.sent()];
                    case 4: return [4 /*yield*/, new Promise(function (resolve, reject) { return client.ZRANGEBYSCORE(key, inclusiveMinScore, inclusiveMaxScore, 'LIMIT', options.offset, options.count, function (err, items) { return err ? reject(err) : resolve(items); }); })];
                    case 5: return [2 /*return*/, _a.sent()];
                    case 6: return [3 /*break*/, 9];
                    case 7:
                        err_14 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 8:
                        _a.sent();
                        throw err_14;
                    case 9: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.zrevrangebyscore = function (key, inclusiveMaxScore, inclusiveMinScore, options) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_15;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 7, , 9]);
                        if (!(options == undefined)) return [3 /*break*/, 4];
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.ZREVRANGEBYSCORE(key, inclusiveMaxScore, inclusiveMinScore, function (err, items) { return err ? reject(err) : resolve(items); }); })];
                    case 3: return [2 /*return*/, _a.sent()];
                    case 4: return [4 /*yield*/, new Promise(function (resolve, reject) { return client.ZREVRANGEBYSCORE(key, inclusiveMaxScore, inclusiveMinScore, 'LIMIT', options.offset, options.count, function (err, items) { return err ? reject(err) : resolve(items); }); })];
                    case 5: return [2 /*return*/, _a.sent()];
                    case 6: return [3 /*break*/, 9];
                    case 7:
                        err_15 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 8:
                        _a.sent();
                        throw err_15;
                    case 9: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.zrem = function (key, members) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_16;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.ZREM(key, members, function (err) { return err ? reject(err) : resolve(); }); })];
                    case 3: return [2 /*return*/, _a.sent()];
                    case 4:
                        err_16 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_16;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.geoadd = function (key, values) {
        return __awaiter(this, void 0, void 0, function () {
            var client, params, err_17;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (values.length == 0)
                            return [2 /*return*/];
                        return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        params = [].concat.apply([], values.map(function (v) { return [v.lon, v.lat, v.value]; }));
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 6]);
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.GEOADD(key, params, function (err) { return err ? reject(err) : resolve(); }); })];
                    case 3:
                        _a.sent();
                        return [3 /*break*/, 6];
                    case 4:
                        err_17 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 5:
                        _a.sent();
                        throw err_17;
                    case 6: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.georadius = function (key, lon, lat, radius, unit, options) {
        return __awaiter(this, void 0, void 0, function () {
            var client, err_18;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.ensureClient()];
                    case 1:
                        client = _a.sent();
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 7, , 9]);
                        if (!(options == undefined)) return [3 /*break*/, 4];
                        return [4 /*yield*/, new Promise(function (resolve, reject) { return client.GEORADIUS(key, lon, lat, radius, unit, function (err, items) { return err ? reject(err) : resolve(items); }); })];
                    case 3: return [2 /*return*/, _a.sent()];
                    case 4: return [4 /*yield*/, new Promise(function (resolve, reject) { return client.GEORADIUS(key, lon, lat, radius, unit, 'COUNT', options.count, function (err, items) { return err ? reject(err) : resolve(items); }); })];
                    case 5: return [2 /*return*/, _a.sent()];
                    case 6: return [3 /*break*/, 9];
                    case 7:
                        err_18 = _a.sent();
                        return [4 /*yield*/, this.uninit()];
                    case 8:
                        _a.sent();
                        throw err_18;
                    case 9: return [2 /*return*/];
                }
            });
        });
    };
    CacheClient.prototype.multi = function () {
        var that = this;
        var MultiCacheClient = /** @class */ (function () {
            function MultiCacheClient() {
                this.fns = [];
            }
            MultiCacheClient.prototype.set = function (key, value) {
                //console.info(`MULTI REDIS SET: ${key} ${value}`);
                this.fns.push(function (m) { m.SET(key, value); return m; });
            };
            MultiCacheClient.prototype.get = function (key) {
                //console.info(`MULTI REDIS GET: ${key}`);
                this.fns.push(function (m) { m.GET(key); return m; });
            };
            MultiCacheClient.prototype.del = function (keys) {
                //console.info(`MULTI REDIS DEL: ${keys.join(', ')}`);
                this.fns.push(function (m) { m.DEL(keys); return m; });
            };
            MultiCacheClient.prototype.expire = function (key, seconds) {
                this.fns.push(function (m) { m.EXPIRE(key, seconds); return m; });
            };
            MultiCacheClient.prototype.exists = function (key) {
                //console.info(`MULTI REDIS EXISTS: ${key}`);
                this.fns.push(function (m) { m.EXISTS(key); return m; });
            };
            MultiCacheClient.prototype.zadd = function (key, values) {
                //console.info(`MULTI REDIS ZADD: ${key}`);
                if (values.length == 0)
                    return;
                var params = values.reduce(function (sum, i) { return __spreadArrays(sum, i); }, []);
                this.fns.push(function (m) { m.ZADD(key, params); return m; });
            };
            MultiCacheClient.prototype.hget = function (key, field) {
                //console.info(`MULTI REDIS HGET: ${key}`);
                this.fns.push(function (m) { m.HGET(key, field); return m; });
            };
            MultiCacheClient.prototype.hmget = function (key, fields) {
                //console.info(`MULTI REDIS HMGET: ${key}`);
                this.fns.push(function (m) { m.HMGET(key, fields); return m; });
            };
            MultiCacheClient.prototype.hset = function (key, field, value) {
                //console.info(`MULTI REDIS HSET: ${key}`);
                this.fns.push(function (m) { m.HSET(key, field, value); return m; });
            };
            MultiCacheClient.prototype.hmset = function (key, values) {
                //console.info(`MULTI REDIS HMSET: ${key}`);
                var params = values.reduce(function (sum, i) { return __spreadArrays(sum, i); }, []);
                this.fns.push(function (m) { m.HMSET(key, params); return m; });
            };
            MultiCacheClient.prototype.hdel = function (key, fields) {
                //console.info(`MULTI REDIS HDEL: ${key}`);
                this.fns.push(function (m) { m.HDEL(key, fields); return m; });
            };
            MultiCacheClient.prototype.zremrangebyscore = function (key, minScore, inclusiveMaxScore) {
                //console.info(`MULTI REDIS ZREMRNAGEBYSCORE: ${key}`);
                this.fns.push(function (m) { m.ZREMRANGEBYSCORE(key, minScore, inclusiveMaxScore); return m; });
            };
            MultiCacheClient.prototype.zrangebyscore = function (key, inclusiveMinScore, inclusiveMaxScore, options) {
                //console.info(`MULTI REDIS ZRANGEBYSCORE: ${key}`);
                if (options == undefined) {
                    this.fns.push(function (m) { m.ZRANGEBYSCORE(key, inclusiveMinScore, inclusiveMaxScore); return m; });
                }
                else {
                    this.fns.push(function (m) { m.ZRANGEBYSCORE(key, inclusiveMinScore, inclusiveMaxScore, 'LIMIT', options.offset, options.count); return m; });
                }
            };
            MultiCacheClient.prototype.zrevrangebyscore = function (key, inclusiveMinScore, inclusiveMaxScore, options) {
                //console.info(`MULTI REDIS ZREVRANGEBYSCORE: ${key}`);
                if (options == undefined) {
                    this.fns.push(function (m) { m.ZREVRANGEBYSCORE(key, inclusiveMaxScore, inclusiveMinScore); return m; });
                }
                else {
                    this.fns.push(function (m) { m.ZREVRANGEBYSCORE(key, inclusiveMaxScore, inclusiveMinScore, 'LIMIT', options.offset, options.count); return m; });
                }
            };
            MultiCacheClient.prototype.zrem = function (key, members) {
                this.fns.push(function (m) { m.ZREM(key, members); return m; });
            };
            MultiCacheClient.prototype.geoadd = function (key, values) {
                if (values.length == 0)
                    return;
                var params = [].concat.apply([], values.map(function (v) { return [v.lon, v.lat, v.value]; }));
                this.fns.push(function (m) { m.GEOADD(key, params); return m; });
            };
            MultiCacheClient.prototype.georadius = function (key, lon, lat, radius, unit, options) {
                if (options == undefined) {
                    this.fns.push(function (m) { m.GEORADIUS(key, lon, lat, radius, unit); return m; });
                }
                else {
                    this.fns.push(function (m) { m.GEORADIUS(key, lon, lat, radius, unit, 'COUNT', options.count); return m; });
                }
            };
            MultiCacheClient.prototype.exec = function () {
                return __awaiter(this, void 0, void 0, function () {
                    var multi;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0: return [4 /*yield*/, that.ensureClient()];
                            case 1:
                                multi = (_a.sent()).multi();
                                this.fns.forEach(function (fn) { multi = fn(multi); });
                                return [2 /*return*/, new Promise(function (resolve, reject) {
                                        multi.exec(function (err, replies) {
                                            if (err) {
                                                reject(err);
                                            }
                                            else {
                                                resolve(replies.map(function (r) { return r.toString(); }));
                                            }
                                        });
                                    })];
                        }
                    });
                });
            };
            return MultiCacheClient;
        }());
        return new MultiCacheClient();
    };
    return CacheClient;
}());
exports.default = CacheClient;
;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi9zcmMvaW5kZXgudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7QUFBQSw2QkFBK0I7QUEyQy9CO0lBSUUscUJBQVksUUFBZ0I7UUFBNUIsaUJBRUM7UUFFRCxTQUFJLEdBQUcsVUFBTyxPQUErQjs7Ozs7d0JBRXJDLE1BQU0sR0FBRyxLQUFLLENBQUMsWUFBWSxDQUFDOzRCQUNoQyxHQUFHLEVBQUUsSUFBSSxDQUFDLFNBQVM7NEJBQ25CLGVBQWUsRUFBRSxJQUFJO3lCQW9CdEIsQ0FBQyxDQUFDOzs7O3dCQUVELHlDQUF5Qzt3QkFDekMscUJBQU0sSUFBSSxPQUFPLENBQUMsVUFBQyxPQUFPLEVBQUUsTUFBTTtnQ0FDaEMsTUFBTSxDQUFDLEVBQUUsQ0FBQyxLQUFLLEVBQUUsVUFBQSxHQUFHLElBQUksT0FBQSxHQUFHLENBQUMsQ0FBQyxDQUFDLE9BQU8sQ0FBQyxHQUFHLENBQUMsQ0FBQyxDQUFDLENBQUMsTUFBTSxFQUFFLEVBQTdCLENBQTZCLENBQUMsQ0FBQztnQ0FDdkQsTUFBTSxDQUFDLEVBQUUsQ0FBQyxPQUFPLEVBQUUsY0FBUSxPQUFPLEVBQUUsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDOzRCQUMzQyxDQUFDLENBQUMsRUFBQTs7d0JBSkYseUNBQXlDO3dCQUN6QyxTQUdFLENBQUM7d0JBQ0gsK0NBQStDO3dCQUMvQyxJQUFJLENBQUMsT0FBTyxHQUFHLE1BQU0sQ0FBQzs7Ozt3QkFFdEIsdUVBQXVFO3dCQUN2RSxJQUFJLENBQUMsT0FBTyxHQUFHLFNBQVMsQ0FBQzt3QkFDekIsTUFBTSxLQUFHLENBQUM7Ozs7YUFFYixDQUFDO1FBRUYsV0FBTSxHQUFHOzs7Z0JBQ1AsSUFBSSxJQUFJLENBQUMsT0FBTyxJQUFJLFNBQVMsRUFBRTtvQkFDdkIsV0FBUyxJQUFJLENBQUMsT0FBTyxDQUFDO29C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QyxBQXRYRCxJQXNYQzs7QUFBQSxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0ICogYXMgUmVkaXMgZnJvbSAncmVkaXMnO1xuXG5leHBvcnQgdHlwZSBJTXVsdGlDYWNoZUNsaWVudCA9IHtcbiAgc2V0KGtleTogc3RyaW5nLCB2YWx1ZTogc3RyaW5nKTogdm9pZDtcbiAgZ2V0KGtleTogc3RyaW5nKTogdm9pZDtcbiAgZGVsKGtleXM6IHN0cmluZ1tdKTogdm9pZDtcbiAgZXhpc3RzKGtleTogc3RyaW5nKTogdm9pZDtcbiAgaGdldChrZXk6IHN0cmluZywgZmllbGQ6IHN0cmluZyk6IHZvaWQ7XG4gIGhtZ2V0KGtleTogc3RyaW5nLCBmaWVsZHM6IHN0cmluZ1tdKTogdm9pZDtcbiAgaHNldChrZXk6IHN0cmluZywgZmllbGQ6IHN0cmluZywgdmFsdWU6IHN0cmluZyk6IHZvaWQ7XG4gIGhtc2V0KGtleTogc3RyaW5nLCB2YWx1ZXM6IFtzdHJpbmcsIHN0cmluZ11bXSk6IHZvaWQ7XG4gIGV4cGlyZShrZXk6IHN0cmluZywgc2Vjb25kczogbnVtYmVyKTogdm9pZDtcbiAgaGRlbChrZXk6IHN0cmluZywgZmllbGRzOiBzdHJpbmdbXSk6IHZvaWQ7XG4gIHphZGQoa2V5OiBzdHJpbmcsIHZhbHVlczogW3N0cmluZywgc3RyaW5nXVtdKTogdm9pZDtcbiAgenJlbXJhbmdlYnlzY29yZShrZXk6IHN0cmluZywgbWluU2NvcmU6IHN0cmluZywgaW5jbHVzaXZlTWF4U2NvcmU6IHN0cmluZyk6IHZvaWQ7XG4gIHpyYW5nZWJ5c2NvcmUoa2V5OiBzdHJpbmcsIGluY2x1c2l2ZU1pblNjb3JlOiBzdHJpbmcsIGluY2x1c2l2ZU1heFNjb3JlOiBzdHJpbmcsIG9wdGlvbnM/OiB7IG9mZnNldDogbnVtYmVyLCBjb3VudDogbnVtYmVyIH0pOiB2b2lkO1xuICB6cmV2cmFuZ2VieXNjb3JlKGtleTogc3RyaW5nLCBpbmNsdXNpdmVNaW5TY29yZTogc3RyaW5nLCBpbmNsdXNpdmVNYXhTY29yZTogc3RyaW5nLCBvcHRpb25zPzogeyBvZmZzZXQ6IG51bWJlciwgY291bnQ6IG51bWJlciB9KTogdm9pZDtcbiAgenJlbShrZXk6IHN0cmluZywgbWVtYmVyczogc3RyaW5nW10pOiB2b2lkO1xuICBnZW9hZGQoa2V5OiBzdHJpbmcsIHZhbHVlczogeyBsb246IG51bWJlciwgbGF0OiBudW1iZXIsIHZhbHVlOiBzdHJpbmcgfVtdKTogdm9pZDtcbiAgZ2VvcmFkaXVzKGtleTogc3RyaW5nLCBsb246IG51bWJlciwgbGF0OiBudW1iZXIsIHJhZGl1czogbnVtYmVyLCB1bml0OiAnbScgfCAna20nIHwgJ2Z0JyB8ICdtaScsIG9wdGlvbnM/OiB7IGNvdW50OiBudW1iZXIgfSk6IHZvaWQ7XG4gIGV4ZWMoKTogUHJvbWlzZTxzdHJpbmdbXT47XG59O1xuXG5leHBvcnQgdHlwZSBJQ2FjaGVDbGllbnQgPSB7XG4gIHNldChrZXk6IHN0cmluZywgdmFsdWU6IHN0cmluZyk6IFByb21pc2U8dm9pZD47XG4gIGdldChrZXk6IHN0cmluZyk6IFByb21pc2U8c3RyaW5nIHwgdW5kZWZpbmVkPjtcbiAgZGVsKGtleXM6IHN0cmluZ1tdKTogUHJvbWlzZTx2b2lkPjtcbiAgZXhpc3RzKGtleTogc3RyaW5nKTogUHJvbWlzZTxib29sZWFuPjtcbiAgaGdldChrZXk6IHN0cmluZywgZmllbGQ6IHN0cmluZyk6IFByb21pc2U8c3RyaW5nIHwgdW5kZWZpbmVkPjtcbiAgaG1nZXQoa2V5OiBzdHJpbmcsIGZpZWxkczogc3RyaW5nW10pOiBQcm9taXNlPChzdHJpbmcgfCB1bmRlZmluZWQpW10+O1xuICBoc2V0KGtleTogc3RyaW5nLCBmaWVsZDogc3RyaW5nLCB2YWx1ZTogc3RyaW5nKTogUHJvbWlzZTx2b2lkPjtcbiAgaG1zZXQoa2V5OiBzdHJpbmcsIHZhbHVlczogW3N0cmluZywgc3RyaW5nXVtdKTogUHJvbWlzZTx2b2lkPjtcbiAgaGRlbChrZXk6IHN0cmluZywgZmllbGRzOiBzdHJpbmdbXSk6IFByb21pc2U8dm9pZD47XG4gIHphZGQoa2V5OiBzdHJpbmcsIHZhbHVlczogW3N0cmluZywgc3RyaW5nXVtdKTogUHJvbWlzZTx2b2lkPjtcbiAgenJlbXJhbmdlYnlzY29yZShrZXk6IHN0cmluZywgbWluU2NvcmU6IHN0cmluZywgaW5jbHVzaXZlTWF4U2NvcmU6IHN0cmluZyk6IFByb21pc2U8dm9pZD47XG4gIHpyYW5nZWJ5c2NvcmUoa2V5OiBzdHJpbmcsIGluY2x1c2l2ZU1pblNjb3JlOiBzdHJpbmcsIGluY2x1c2l2ZU1heFNjb3JlOiBzdHJpbmcsIG9wdGlvbnM/OiB7IG9mZnNldDogbnVtYmVyLCBjb3VudDogbnVtYmVyIH0pOiBQcm9taXNlPHN0cmluZ1tdPjtcbiAgenJldnJhbmdlYnlzY29yZShrZXk6IHN0cmluZywgaW5jbHVzaXZlTWluU2NvcmU6IHN0cmluZywgaW5jbHVzaXZlTWF4U2NvcmU6IHN0cmluZywgb3B0aW9ucz86IHsgb2Zmc2V0OiBudW1iZXIsIGNvdW50OiBudW1iZXIgfSk6IFByb21pc2U8c3RyaW5nW10+O1xuICB6cmVtKGtleTogc3RyaW5nLCBtZW1iZXJzOiBzdHJpbmdbXSk6IFByb21pc2U8dm9pZD47XG4gIGdlb2FkZChrZXk6IHN0cmluZywgdmFsdWVzOiB7IGxvbjogbnVtYmVyLCBsYXQ6IG51bWJlciwgdmFsdWU6IHN0cmluZyB9W10pOiBQcm9taXNlPHZvaWQ+O1xuICBnZW9yYWRpdXMoa2V5OiBzdHJpbmcsIGxvbjogbnVtYmVyLCBsYXQ6IG51bWJlciwgcmFkaXVzOiBudW1iZXIsIHVuaXQ6ICdtJyB8ICdrbScgfCAnZnQnIHwgJ21pJywgb3B0aW9ucz86IHsgY291bnQ6IG51bWJlciB9KTogUHJvbWlzZTwoc3RyaW5nIHwgW3N0cmluZywgc3RyaW5nIHwgW3N0cmluZywgc3RyaW5nXV0pW10+O1xuICBtdWx0aSgpOiBJTXVsdGlDYWNoZUNsaWVudDtcbn1cblxuZXhwb3J0IGRlZmF1bHQgY2xhc3MgQ2FjaGVDbGllbnQgaW1wbGVtZW50cyBJQ2FjaGVDbGllbnQge1xuICBwcml2YXRlIF9yZWRpc1VybDogc3RyaW5nO1xuICBwcml2YXRlIF9jbGllbnQ6IFJlZGlzLlJlZGlzQ2xpZW50IHwgdW5kZWZpbmVkO1xuXG4gIGNvbnN0cnVjdG9yKHJlZGlzVXJsOiBzdHJpbmcpIHtcbiAgICB0aGlzLl9yZWRpc1VybCA9IHJlZGlzVXJsO1xuICB9XG5cbiAgaW5pdCA9IGFzeW5jIChvcHRpb25zPzogeyBmbHVzaEFsbDogYm9vbGVhbiB9KSA9PiB7XG4gICAgLy9jb25zb2xlLmxvZyhgQ09OTkVDVElORyBUTyBSRURJUy4uLi4gXCIke2NvbmZpZygnUkVESVNfVVJMJyl9XCJgKTtcbiAgICBjb25zdCBjbGllbnQgPSBSZWRpcy5jcmVhdGVDbGllbnQoe1xuICAgICAgdXJsOiB0aGlzLl9yZWRpc1VybCxcbiAgICAgIGNvbm5lY3RfdGltZW91dDogNTAwMCwgLy8gZm9yIHRlc3RpbmcsIHVzZSByZXRyeSBzdHJhdGVneSBmb3IgcHJvZHVjdGlvbiBwdXJwb3Nlc1xuICAgICAgLypcbiAgICAgIHJldHJ5X3N0cmF0ZWd5OiBvcHRpb25zID0+IHtcbiAgICAgICAgaWYgKG9wdGlvbnMuZXJyb3IgJiYgb3B0aW9ucy5lcnJvci5jb2RlID09PSAnRUNPTk5SRUZVU0VEJykge1xuICAgICAgICAgIC8vIEVuZCByZWNvbm5lY3Rpbmcgb24gYSBzcGVjaWZpYyBlcnJvciBhbmQgZmx1c2ggYWxsIGNvbW1hbmRzIHdpdGhcbiAgICAgICAgICAvLyBhIGluZGl2aWR1YWwgZXJyb3JcbiAgICAgICAgICByZXR1cm4gbmV3IEVycm9yKCdSZWRpcyByZWZ1c2VkIHRoZSBjb25uZWN0aW9uJyk7XG4gICAgICAgIH0gZWxzZSBpZiAob3B0aW9ucy50b3RhbF9yZXRyeV90aW1lID4gMTAwMCAqIDYwICogNjApIHtcbiAgICAgICAgICAvLyBFbmQgcmVjb25uZWN0aW5nIGFmdGVyIGEgc3BlY2lmaWMgdGltZW91dCBhbmQgZmx1c2ggYWxsIGNvbW1hbmRzXG4gICAgICAgICAgLy8gd2l0aCBhIGluZGl2aWR1YWwgZXJyb3JcbiAgICAgICAgICByZXR1cm4gbmV3IEVycm9yKCdSZWRpcyByZXRyeSB0aW1lIGV4aGF1c3RlZCcpO1xuICAgICAgICB9IGVsc2UgaWYgKG9wdGlvbnMuYXR0ZW1wdCA+IDEwKSB7XG4gICAgICAgICAgLy8gRW5kIHJlY29ubmVjdGluZyB3aXRoIGJ1aWx0IGluIGVycm9yXG4gICAgICAgICAgcmV0dXJuIG5ldyBFcnJvcignUmVkaXMgQXR0ZW1wdHMgZXhoYXVzdGVkJyk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgLy8gcmVjb25uZWN0IGFmdGVyXG4gICAgICAgICAgcmV0dXJuIE1hdGgubWluKG9wdGlvbnMuYXR0ZW1wdCAqIDEwMCwgMzAwMCk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICAgICovXG4gICAgfSk7XG4gICAgdHJ5IHtcbiAgICAgIC8vY29uc29sZS5sb2coJ1JFRElTIENMSUVOVCBDUkVBVEVELi4uJyk7XG4gICAgICBhd2FpdCBuZXcgUHJvbWlzZSgocmVzb2x2ZSwgcmVqZWN0KSA9PiB7XG4gICAgICAgIGNsaWVudC5vbignZW5kJywgZXJyID0+IGVyciA/IHJlc29sdmUoZXJyKSA6IHJlamVjdCgpKTtcbiAgICAgICAgY2xpZW50Lm9uKCdyZWFkeScsICgpID0+IHsgcmVzb2x2ZSgpOyB9KTtcbiAgICAgIH0pO1xuICAgICAgLy9jb25zb2xlLmxvZygnUkVESVMgQ09OTkVDVElPTiBFU1RBQklMSVNIRUQnKTtcbiAgICAgIHRoaXMuX2NsaWVudCA9IGNsaWVudDtcbiAgICB9IGNhdGNoIChlcnIpIHtcbiAgICAgIC8vY29uc29sZS5lcnJvcihgRkFJTEVEIFRPIENPTk5FQ1QgVE8gUkVESVMgLSAke0pTT04uc3RyaW5naWZ5KGVycil9YCk7XG4gICAgICB0aGlzLl9jbGllbnQgPSB1bmRlZmluZWQ7XG4gICAgICB0aHJvdyBlcnI7XG4gICAgfVxuICB9O1xuXG4gIHVuaW5pdCA9IGFzeW5jICgpID0+IHtcbiAgICBpZiAodGhpcy5fY2xpZW50ICE9IHVuZGVmaW5lZCkge1xuICAgICAgY29uc3QgY2xpZW50ID0gdGhpcy5fY2xpZW50O1xuICAgICAgdGhpcy5fY2xpZW50ID0gdW5kZWZpbmVkO1xuICAgICAgcmV0dXJuIG5ldyBQcm9taXNlPHZvaWQ+KChyZXNvbHZlLCByZWplY3QpID0+IGNsaWVudC5xdWl0KGVyciA9PiBlcnIgPyByZWplY3QoZXJyKSA6IHJlc29sdmUoKSkpO1xuICAgIH1cbiAgfTtcblxuICByZXNldCA9IGFzeW5jICgpID0+IHtcbiAgICBpZiAocHJvY2Vzcy5lbnYuTk9ERV9FTlYgIT0gJ3Rlc3QnKSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoJ0VSUk9SISEgY2FjaGUgcmVzZXQgc2hvdWxkIG9ubHkgYmUgY2FsbGVkIGluIHRoZSB0ZXN0IGVudmlyb25tZW50ISBpZ25vcmluZycpO1xuICAgIH1cbiAgICBpZiAodGhpcy5fY2xpZW50ID09PSB1bmRlZmluZWQpIHRocm93IG5ldyBFcnJvcigncmVkaXMgY2xpZW50IG5vdCBpbml0aWFsaXplZCcpO1xuICAgIGNvbnN0IGNsaWVudCA9IHRoaXMuX2NsaWVudDtcbiAgICByZXR1cm4gbmV3IFByb21pc2UoKHJlc29sdmUsIHJlamVjdCkgPT5cbiAgICAgIGNsaWVudC5mbHVzaGFsbChlcnIgPT4gZXJyID8gcmVqZWN0KGVycikgOiByZXNvbHZlKCkpKTtcbiAgfVxuXG4gIGVuc3VyZUNsaWVudCA9IGFzeW5jICgpOiBQcm9taXNlPFJlZGlzLlJlZGlzQ2xpZW50PiA9PiB7XG4gICAgaWYgKHRoaXMuX2NsaWVudCA9PSB1bmRlZmluZWQpIHtcbiAgICAgIGF3YWl0IHRoaXMuaW5pdCgpO1xuICAgICAgaWYgKHRoaXMuX2NsaWVudCA9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgdGhyb3cgbmV3IEVycm9yKCdyZWRpcyBjbGllbnQgbm90IGluaXRpYWxpemVkJyk7XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiB0aGlzLl9jbGllbnQ7XG4gIH1cblxuICAvL2RlbGV0ZSBieSBwcmVmaXhcbiAgLy9jbGllbnQuRVZBTChgcmV0dXJuIHJlZGlzLmNhbGwoJ2RlbCcsICdkZWZhdWx0S2V5JywgdW5wYWNrKHJlZGlzLmNhbGwoJ2tleXMnLCBBUkdWWzFdKSkpYCwgMCwgJ3ByZWZpeDoqJyk7XG5cbiAgYXN5bmMgc2V0KGtleTogc3RyaW5nLCB2YWx1ZTogc3RyaW5nKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgLy9jb25zb2xlLmluZm8oYFJFRElTIFNFVDogJHtrZXl9ICR7dmFsdWV9YCk7XG4gICAgY29uc3QgY2xpZW50ID0gYXdhaXQgdGhpcy5lbnN1cmVDbGllbnQoKTtcbiAgICB0cnkge1xuICAgICAgcmV0dXJuIG5ldyBQcm9taXNlPHZvaWQ+KChyZXNvbHZlLCByZWplY3QpID0+IGNsaWVudC5zZXQoa2V5LCB2YWx1ZSwgZXJyID0+IGVyciA/IHJlamVjdChlcnIpIDogcmVzb2x2ZSgpKSk7XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICBhd2FpdCB0aGlzLnVuaW5pdCgpO1xuICAgICAgdGhyb3cgZXJyO1xuICAgIH1cbiAgfVxuICBhc3luYyBnZXQoa2V5OiBzdHJpbmcpOiBQcm9taXNlPHN0cmluZyB8IHVuZGVmaW5lZD4ge1xuICAgIC8vY29uc29sZS5pbmZvKGBSRURJUyBHRVQ6ICR7a2V5fWApO1xuICAgIGNvbnN0IGNsaWVudCA9IGF3YWl0IHRoaXMuZW5zdXJlQ2xpZW50KCk7XG4gICAgdHJ5IHtcbiAgICAgIGNvbnN0IHJldCA9IGF3YWl0IG5ldyBQcm9taXNlPHN0cmluZz4oKHJlc29sdmUsIHJlamVjdCkgPT4gY2xpZW50LmdldChrZXksIChlcnIsIHJlcykgPT4gZXJyID8gcmVqZWN0KGVycikgOiByZXNvbHZlKHJlcykpKTtcbiAgICAgIHJldHVybiByZXQgPT09IG51bGwgPyB1bmRlZmluZWQgOiByZXQ7XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICBhd2FpdCB0aGlzLnVuaW5pdCgpO1xuICAgICAgdGhyb3cgZXJyO1xuICAgIH1cbiAgfVxuICBhc3luYyBkZWwoa2V5czogc3RyaW5nW10pOiBQcm9taXNlPHZvaWQ+IHtcbiAgICAvL2NvbnNvbGUuaW5mbyhgUkVESVMgREVMOiAke2tleXMuam9pbignLCAnKX1gKTtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICBhd2FpdCBuZXcgUHJvbWlzZTx2b2lkPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuREVMKGtleXMsIGVyciA9PiBlcnIgPyByZWplY3QoZXJyKSA6IHJlc29sdmUoKSkpO1xuICAgIH0gY2F0Y2ggKGVycikge1xuICAgICAgYXdhaXQgdGhpcy51bmluaXQoKTtcbiAgICAgIHRocm93IGVycjtcbiAgICB9XG4gIH1cbiAgYXN5bmMgZXhwaXJlKGtleTogc3RyaW5nLCBzZWNvbmRzOiBudW1iZXIpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICBhd2FpdCBuZXcgUHJvbWlzZTx2b2lkPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuRVhQSVJFKGtleSwgc2Vjb25kcywgZXJyID0+IGVyciA/IHJlamVjdChlcnIpIDogcmVzb2x2ZSgpKSk7XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICBhd2FpdCB0aGlzLnVuaW5pdCgpO1xuICAgICAgdGhyb3cgZXJyO1xuICAgIH1cbiAgfVxuICBhc3luYyBleGlzdHMoa2V5OiBzdHJpbmcpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICAvL2NvbnNvbGUuaW5mbyhgUkVESVMgRVhJU1RTOiAke2tleX1gKTtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICByZXR1cm4gYXdhaXQgbmV3IFByb21pc2U8Ym9vbGVhbj4oKHJlc29sdmUsIHJlamVjdCkgPT4gY2xpZW50LkVYSVNUUyhrZXksIChlcnIsIHJlcykgPT4gZXJyID8gcmVqZWN0KGVycikgOiByZXNvbHZlKHJlcyAhPSAwKSkpO1xuICAgIH0gY2F0Y2ggKGVycikge1xuICAgICAgYXdhaXQgdGhpcy51bmluaXQoKTtcbiAgICAgIHRocm93IGVycjtcbiAgICB9XG4gIH1cbiAgYXN5bmMgemFkZChrZXk6IHN0cmluZywgdmFsdWVzOiBbc3RyaW5nLCBzdHJpbmddW10pOiBQcm9taXNlPHZvaWQ+IHtcbiAgICAvL2NvbnNvbGUuaW5mbyhgUkVESVMgWkFERDogJHtrZXl9YCk7XG4gICAgaWYgKHZhbHVlcy5sZW5ndGggPT0gMCkgcmV0dXJuO1xuICAgIGNvbnN0IHBhcmFtcyA9IHZhbHVlcy5yZWR1Y2UoKHN1bSwgaSkgPT4gWy4uLnN1bSwgLi4uaV0sIFtdIGFzIHN0cmluZ1tdKTtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICBhd2FpdCBuZXcgUHJvbWlzZTx2b2lkPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuWkFERChrZXksIHBhcmFtcywgZXJyID0+IGVyciA/IHJlamVjdChlcnIpIDogcmVzb2x2ZSgpKSk7XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICBhd2FpdCB0aGlzLnVuaW5pdCgpO1xuICAgICAgdGhyb3cgZXJyO1xuICAgIH1cbiAgfVxuICBhc3luYyBoZ2V0KGtleTogc3RyaW5nLCBmaWVsZDogc3RyaW5nKTogUHJvbWlzZTxzdHJpbmcgfCB1bmRlZmluZWQ+IHtcbiAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgSEdFVDogJHtrZXl9YCk7XG4gICAgY29uc3QgY2xpZW50ID0gYXdhaXQgdGhpcy5lbnN1cmVDbGllbnQoKTtcbiAgICB0cnkge1xuICAgICAgY29uc3QgcmV0ID0gYXdhaXQgbmV3IFByb21pc2U8c3RyaW5nPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuSEdFVChrZXksIGZpZWxkLCAoZXJyLCByZXMpID0+IGVyciA/IHJlamVjdChlcnIpIDogcmVzb2x2ZShyZXMpKSk7XG4gICAgICByZXR1cm4gcmV0ID09PSBudWxsID8gdW5kZWZpbmVkIDogcmV0O1xuICAgIH0gY2F0Y2ggKGVycikge1xuICAgICAgYXdhaXQgdGhpcy51bmluaXQoKTtcbiAgICAgIHRocm93IGVycjtcbiAgICB9XG4gIH1cbiAgYXN5bmMgaG1nZXQoa2V5OiBzdHJpbmcsIGZpZWxkczogc3RyaW5nW10pOiBQcm9taXNlPChzdHJpbmcgfCB1bmRlZmluZWQpW10+IHtcbiAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgSE1HRVQ6ICR7a2V5fWApO1xuICAgIGNvbnN0IGNsaWVudCA9IGF3YWl0IHRoaXMuZW5zdXJlQ2xpZW50KCk7XG4gICAgdHJ5IHtcbiAgICAgIGNvbnN0IHJldCA9IGF3YWl0IG5ldyBQcm9taXNlPHN0cmluZ1tdPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuSE1HRVQoa2V5LCBmaWVsZHMsIChlcnIsIHJlcykgPT4gZXJyID8gcmVqZWN0KGVycikgOiByZXNvbHZlKHJlcykpKTtcbiAgICAgIHJldHVybiByZXQubWFwKHIgPT4gciA9PT0gbnVsbCA/IHVuZGVmaW5lZCA6IHIpO1xuICAgIH0gY2F0Y2ggKGVycikge1xuICAgICAgYXdhaXQgdGhpcy51bmluaXQoKTtcbiAgICAgIHRocm93IGVycjtcbiAgICB9XG4gIH1cbiAgYXN5bmMgaHNldChrZXk6IHN0cmluZywgZmllbGQ6IHN0cmluZywgdmFsdWU6IHN0cmluZyk6IFByb21pc2U8dm9pZD4ge1xuICAgIC8vY29uc29sZS5pbmZvKGBSRURJUyBIU0VUOiAke2tleX1gKTtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICBhd2FpdCBuZXcgUHJvbWlzZTx2b2lkPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuSFNFVChrZXksIGZpZWxkLCB2YWx1ZSwgZXJyID0+IGVyciA/IHJlamVjdChlcnIpIDogcmVzb2x2ZSgpKSk7XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICBhd2FpdCB0aGlzLnVuaW5pdCgpO1xuICAgICAgdGhyb3cgZXJyO1xuICAgIH1cbiAgfVxuICBhc3luYyBobXNldChrZXk6IHN0cmluZywgdmFsdWVzOiBbc3RyaW5nLCBzdHJpbmddW10pOiBQcm9taXNlPHZvaWQ+IHtcbiAgICAvL2NvbnNvbGUuaW5mbyhgUkVESVMgSE1TRVQ6ICR7a2V5fWApO1xuICAgIGNvbnN0IHBhcmFtcyA9IHZhbHVlcy5yZWR1Y2UoKHN1bSwgaSkgPT4gWy4uLnN1bSwgLi4uaV0sIFtdIGFzIHN0cmluZ1tdKTtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICBhd2FpdCBuZXcgUHJvbWlzZTx2b2lkPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuSE1TRVQoa2V5LCBwYXJhbXMsIGVyciA9PiBlcnIgPyByZWplY3QoZXJyKSA6IHJlc29sdmUoKSkpO1xuICAgIH0gY2F0Y2ggKGVycikge1xuICAgICAgYXdhaXQgdGhpcy51bmluaXQoKTtcbiAgICAgIHRocm93IGVycjtcbiAgICB9XG4gIH1cbiAgYXN5bmMgaGRlbChrZXk6IHN0cmluZywgZmllbGRzOiBzdHJpbmdbXSk6IFByb21pc2U8dm9pZD4ge1xuICAgIC8vY29uc29sZS5pbmZvKGBSRURJUyBIREVMOiAke2tleX1gKTtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICBhd2FpdCBuZXcgUHJvbWlzZTx2b2lkPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuSERFTChrZXksIGZpZWxkcywgZXJyID0+IGVyciA/IHJlamVjdChlcnIpIDogcmVzb2x2ZSgpKSk7XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICBhd2FpdCB0aGlzLnVuaW5pdCgpO1xuICAgICAgdGhyb3cgZXJyO1xuICAgIH1cbiAgfVxuICBhc3luYyB6cmVtcmFuZ2VieXNjb3JlKGtleTogc3RyaW5nLCBtaW5TY29yZTogc3RyaW5nLCBpbmNsdXNpdmVNYXhTY29yZTogc3RyaW5nKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgLy9jb25zb2xlLmluZm8oYFJFRElTIFpSRU1STkFHRUJZU0NPUkU6ICR7a2V5fWApO1xuICAgIGNvbnN0IGNsaWVudCA9IGF3YWl0IHRoaXMuZW5zdXJlQ2xpZW50KCk7XG4gICAgdHJ5IHtcbiAgICAgIGF3YWl0IG5ldyBQcm9taXNlPHZvaWQ+KChyZXNvbHZlLCByZWplY3QpID0+XG4gICAgICAgIGNsaWVudC5aUkVNUkFOR0VCWVNDT1JFKGtleSwgbWluU2NvcmUsIGluY2x1c2l2ZU1heFNjb3JlLCBlcnIgPT4gZXJyID8gcmVqZWN0KGVycikgOiByZXNvbHZlKCkpKTtcbiAgICB9IGNhdGNoIChlcnIpIHtcbiAgICAgIGF3YWl0IHRoaXMudW5pbml0KCk7XG4gICAgICB0aHJvdyBlcnI7XG4gICAgfVxuICB9XG4gIGFzeW5jIHpyYW5nZWJ5c2NvcmUoa2V5OiBzdHJpbmcsIGluY2x1c2l2ZU1pblNjb3JlOiBzdHJpbmcsIGluY2x1c2l2ZU1heFNjb3JlOiBzdHJpbmcsIG9wdGlvbnM/OiB7IG9mZnNldDogbnVtYmVyLCBjb3VudDogbnVtYmVyIH0pOiBQcm9taXNlPHN0cmluZ1tdPiB7XG4gICAgLy9jb25zb2xlLmluZm8oYFJFRElTIFpSQU5HRUJZU0NPUkU6ICR7a2V5fWApO1xuICAgIGNvbnN0IGNsaWVudCA9IGF3YWl0IHRoaXMuZW5zdXJlQ2xpZW50KCk7XG4gICAgdHJ5IHtcbiAgICAgIGlmIChvcHRpb25zID09IHVuZGVmaW5lZCkge1xuICAgICAgICByZXR1cm4gYXdhaXQgbmV3IFByb21pc2U8c3RyaW5nW10+KChyZXNvbHZlLCByZWplY3QpID0+IGNsaWVudC5aUkFOR0VCWVNDT1JFKFxuICAgICAgICAgIGtleSwgaW5jbHVzaXZlTWluU2NvcmUsIGluY2x1c2l2ZU1heFNjb3JlLCAoZXJyLCBpdGVtcykgPT4gZXJyID8gcmVqZWN0KGVycikgOiByZXNvbHZlKGl0ZW1zKSkpO1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgcmV0dXJuIGF3YWl0IG5ldyBQcm9taXNlPHN0cmluZ1tdPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuWlJBTkdFQllTQ09SRShcbiAgICAgICAgICBrZXksIGluY2x1c2l2ZU1pblNjb3JlLCBpbmNsdXNpdmVNYXhTY29yZSwgJ0xJTUlUJywgb3B0aW9ucy5vZmZzZXQsIG9wdGlvbnMuY291bnQsIChlcnIsIGl0ZW1zKSA9PiBlcnIgPyByZWplY3QoZXJyKSA6IHJlc29sdmUoaXRlbXMpKSk7XG4gICAgICB9XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICBhd2FpdCB0aGlzLnVuaW5pdCgpO1xuICAgICAgdGhyb3cgZXJyO1xuICAgIH1cbiAgfVxuICBhc3luYyB6cmV2cmFuZ2VieXNjb3JlKGtleTogc3RyaW5nLCBpbmNsdXNpdmVNYXhTY29yZTogc3RyaW5nLCBpbmNsdXNpdmVNaW5TY29yZTogc3RyaW5nLCBvcHRpb25zPzogeyBvZmZzZXQ6IG51bWJlciwgY291bnQ6IG51bWJlciB9KTogUHJvbWlzZTxzdHJpbmdbXT4ge1xuICAgIC8vY29uc29sZS5pbmZvKGBSRURJUyBaUkVWUkFOR0VCWVNDT1JFOiAke2tleX1gKTtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICBpZiAob3B0aW9ucyA9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgcmV0dXJuIGF3YWl0IG5ldyBQcm9taXNlPHN0cmluZ1tdPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuWlJFVlJBTkdFQllTQ09SRShcbiAgICAgICAgICBrZXksIGluY2x1c2l2ZU1heFNjb3JlLCBpbmNsdXNpdmVNaW5TY29yZSwgKGVyciwgaXRlbXMpID0+IGVyciA/IHJlamVjdChlcnIpIDogcmVzb2x2ZShpdGVtcykpKTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIHJldHVybiBhd2FpdCBuZXcgUHJvbWlzZTxzdHJpbmdbXT4oKHJlc29sdmUsIHJlamVjdCkgPT4gY2xpZW50LlpSRVZSQU5HRUJZU0NPUkUoXG4gICAgICAgICAga2V5LCBpbmNsdXNpdmVNYXhTY29yZSwgaW5jbHVzaXZlTWluU2NvcmUsICdMSU1JVCcsIG9wdGlvbnMub2Zmc2V0LCBvcHRpb25zLmNvdW50LCAoZXJyLCBpdGVtcykgPT4gZXJyID8gcmVqZWN0KGVycikgOiByZXNvbHZlKGl0ZW1zKSkpO1xuICAgICAgfVxuICAgIH0gY2F0Y2ggKGVycikge1xuICAgICAgYXdhaXQgdGhpcy51bmluaXQoKTtcbiAgICAgIHRocm93IGVycjtcbiAgICB9XG4gIH1cbiAgYXN5bmMgenJlbShrZXk6IHN0cmluZywgbWVtYmVyczogc3RyaW5nW10pOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICByZXR1cm4gYXdhaXQgbmV3IFByb21pc2U8dm9pZD4oKHJlc29sdmUsIHJlamVjdCkgPT4gY2xpZW50LlpSRU0oa2V5LCBtZW1iZXJzLCAoZXJyKSA9PiBlcnIgPyByZWplY3QoZXJyKSA6IHJlc29sdmUoKSkpO1xuICAgIH0gY2F0Y2ggKGVycikge1xuICAgICAgYXdhaXQgdGhpcy51bmluaXQoKTtcbiAgICAgIHRocm93IGVycjtcbiAgICB9XG4gIH1cbiAgYXN5bmMgZ2VvYWRkKGtleTogc3RyaW5nLCB2YWx1ZXM6IHsgbG9uOiBudW1iZXIsIGxhdDogbnVtYmVyLCB2YWx1ZTogc3RyaW5nIH1bXSk6IFByb21pc2U8dm9pZD4ge1xuICAgIGlmICh2YWx1ZXMubGVuZ3RoID09IDApIHJldHVybjtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIGNvbnN0IHBhcmFtcyA9IFtdLmNvbmNhdC5hcHBseShbXSwgdmFsdWVzLm1hcCh2ID0+IFt2Lmxvbiwgdi5sYXQsIHYudmFsdWVdKSkgYXMgKHN0cmluZyB8IG51bWJlcilbXTtcbiAgICB0cnkge1xuICAgICAgYXdhaXQgbmV3IFByb21pc2U8dm9pZD4oKHJlc29sdmUsIHJlamVjdCkgPT4gY2xpZW50LkdFT0FERChrZXksIHBhcmFtcywgZXJyID0+IGVyciA/IHJlamVjdChlcnIpIDogcmVzb2x2ZSgpKSk7XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICBhd2FpdCB0aGlzLnVuaW5pdCgpO1xuICAgICAgdGhyb3cgZXJyO1xuICAgIH1cbiAgfVxuICBhc3luYyBnZW9yYWRpdXMoa2V5OiBzdHJpbmcsIGxvbjogbnVtYmVyLCBsYXQ6IG51bWJlciwgcmFkaXVzOiBudW1iZXIsIHVuaXQ6ICdtJyB8ICdrbScgfCAnZnQnIHwgJ21pJywgb3B0aW9ucz86IHsgY291bnQ6IG51bWJlciB9KTogUHJvbWlzZTwoc3RyaW5nIHwgW3N0cmluZywgc3RyaW5nIHwgW3N0cmluZywgc3RyaW5nXV0pW10+IHtcbiAgICBjb25zdCBjbGllbnQgPSBhd2FpdCB0aGlzLmVuc3VyZUNsaWVudCgpO1xuICAgIHRyeSB7XG4gICAgICBpZiAob3B0aW9ucyA9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgcmV0dXJuIGF3YWl0IG5ldyBQcm9taXNlPChzdHJpbmcgfCBbc3RyaW5nLCBzdHJpbmcgfCBbc3RyaW5nLCBzdHJpbmddXSlbXT4oKHJlc29sdmUsIHJlamVjdCkgPT4gY2xpZW50LkdFT1JBRElVUyhrZXksIGxvbiwgbGF0LCByYWRpdXMsIHVuaXQsIChlcnIsIGl0ZW1zKSA9PiBlcnIgPyByZWplY3QoZXJyKSA6IHJlc29sdmUoaXRlbXMpKSk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICByZXR1cm4gYXdhaXQgbmV3IFByb21pc2U8KHN0cmluZyB8IFtzdHJpbmcsIHN0cmluZyB8IFtzdHJpbmcsIHN0cmluZ11dKVtdPigocmVzb2x2ZSwgcmVqZWN0KSA9PiBjbGllbnQuR0VPUkFESVVTKGtleSwgbG9uLCBsYXQsIHJhZGl1cywgdW5pdCwgJ0NPVU5UJywgb3B0aW9ucy5jb3VudCwgKGVyciwgaXRlbXMpID0+IGVyciA/IHJlamVjdChlcnIpIDogcmVzb2x2ZShpdGVtcykpKTtcbiAgICAgIH1cbiAgICB9IGNhdGNoIChlcnIpIHtcbiAgICAgIGF3YWl0IHRoaXMudW5pbml0KCk7XG4gICAgICB0aHJvdyBlcnI7XG4gICAgfVxuICB9XG4gIG11bHRpKCk6IElNdWx0aUNhY2hlQ2xpZW50IHtcbiAgICBjb25zdCB0aGF0ID0gdGhpcztcbiAgICBjbGFzcyBNdWx0aUNhY2hlQ2xpZW50IGltcGxlbWVudHMgSU11bHRpQ2FjaGVDbGllbnQge1xuICAgICAgZm5zOiAoKG11bHRpOiBSZWRpcy5NdWx0aSkgPT4gUmVkaXMuTXVsdGkpW107XG4gICAgICBjb25zdHJ1Y3RvcigpIHtcbiAgICAgICAgdGhpcy5mbnMgPSBbXTtcbiAgICAgIH1cbiAgICAgIHNldChrZXk6IHN0cmluZywgdmFsdWU6IHN0cmluZyk6IHZvaWQge1xuICAgICAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgU0VUOiAke2tleX0gJHt2YWx1ZX1gKTtcbiAgICAgICAgdGhpcy5mbnMucHVzaChtID0+IHsgbS5TRVQoa2V5LCB2YWx1ZSk7IHJldHVybiBtOyB9KTtcbiAgICAgIH1cbiAgICAgIGdldChrZXk6IHN0cmluZyk6IHZvaWQge1xuICAgICAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgR0VUOiAke2tleX1gKTtcbiAgICAgICAgdGhpcy5mbnMucHVzaChtID0+IHsgbS5HRVQoa2V5KTsgcmV0dXJuIG07IH0pO1xuICAgICAgfVxuICAgICAgZGVsKGtleXM6IHN0cmluZ1tdKTogdm9pZCB7XG4gICAgICAgIC8vY29uc29sZS5pbmZvKGBNVUxUSSBSRURJUyBERUw6ICR7a2V5cy5qb2luKCcsICcpfWApO1xuICAgICAgICB0aGlzLmZucy5wdXNoKG0gPT4geyBtLkRFTChrZXlzKTsgcmV0dXJuIG07IH0pO1xuICAgICAgfVxuICAgICAgZXhwaXJlKGtleTogc3RyaW5nLCBzZWNvbmRzOiBudW1iZXIpOiB2b2lkIHtcbiAgICAgICAgdGhpcy5mbnMucHVzaChtID0+IHsgbS5FWFBJUkUoa2V5LCBzZWNvbmRzKTsgcmV0dXJuIG07IH0pO1xuICAgICAgfVxuICAgICAgZXhpc3RzKGtleTogc3RyaW5nKTogdm9pZCB7XG4gICAgICAgIC8vY29uc29sZS5pbmZvKGBNVUxUSSBSRURJUyBFWElTVFM6ICR7a2V5fWApO1xuICAgICAgICB0aGlzLmZucy5wdXNoKG0gPT4geyBtLkVYSVNUUyhrZXkpOyByZXR1cm4gbTsgfSk7XG4gICAgICB9XG4gICAgICB6YWRkKGtleTogc3RyaW5nLCB2YWx1ZXM6IFtzdHJpbmcsIHN0cmluZ11bXSk6IHZvaWQge1xuICAgICAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgWkFERDogJHtrZXl9YCk7XG4gICAgICAgIGlmICh2YWx1ZXMubGVuZ3RoID09IDApIHJldHVybjtcbiAgICAgICAgY29uc3QgcGFyYW1zID0gdmFsdWVzLnJlZHVjZSgoc3VtLCBpKSA9PiBbLi4uc3VtLCAuLi5pXSwgW10gYXMgc3RyaW5nW10pO1xuICAgICAgICB0aGlzLmZucy5wdXNoKG0gPT4geyBtLlpBREQoa2V5LCBwYXJhbXMpOyByZXR1cm4gbTsgfSk7XG4gICAgICB9XG4gICAgICBoZ2V0KGtleTogc3RyaW5nLCBmaWVsZDogc3RyaW5nKTogdm9pZCB7XG4gICAgICAgIC8vY29uc29sZS5pbmZvKGBNVUxUSSBSRURJUyBIR0VUOiAke2tleX1gKTtcbiAgICAgICAgdGhpcy5mbnMucHVzaChtID0+IHsgbS5IR0VUKGtleSwgZmllbGQpOyByZXR1cm4gbTsgfSk7XG4gICAgICB9XG4gICAgICBobWdldChrZXk6IHN0cmluZywgZmllbGRzOiBzdHJpbmdbXSk6IHZvaWQge1xuICAgICAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgSE1HRVQ6ICR7a2V5fWApO1xuICAgICAgICB0aGlzLmZucy5wdXNoKG0gPT4geyBtLkhNR0VUKGtleSwgZmllbGRzKTsgcmV0dXJuIG07IH0pO1xuICAgICAgfVxuICAgICAgaHNldChrZXk6IHN0cmluZywgZmllbGQ6IHN0cmluZywgdmFsdWU6IHN0cmluZyk6IHZvaWQge1xuICAgICAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgSFNFVDogJHtrZXl9YCk7XG4gICAgICAgIHRoaXMuZm5zLnB1c2gobSA9PiB7IG0uSFNFVChrZXksIGZpZWxkLCB2YWx1ZSk7IHJldHVybiBtOyB9KTtcbiAgICAgIH1cbiAgICAgIGhtc2V0KGtleTogc3RyaW5nLCB2YWx1ZXM6IFtzdHJpbmcsIHN0cmluZ11bXSk6IHZvaWQge1xuICAgICAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgSE1TRVQ6ICR7a2V5fWApO1xuICAgICAgICBjb25zdCBwYXJhbXMgPSB2YWx1ZXMucmVkdWNlKChzdW0sIGkpID0+IFsuLi5zdW0sIC4uLmldLCBbXSBhcyBzdHJpbmdbXSk7XG4gICAgICAgIHRoaXMuZm5zLnB1c2gobSA9PiB7IG0uSE1TRVQoa2V5LCBwYXJhbXMpOyByZXR1cm4gbTsgfSk7XG4gICAgICB9XG4gICAgICBoZGVsKGtleTogc3RyaW5nLCBmaWVsZHM6IHN0cmluZ1tdKTogdm9pZCB7XG4gICAgICAgIC8vY29uc29sZS5pbmZvKGBNVUxUSSBSRURJUyBIREVMOiAke2tleX1gKTtcbiAgICAgICAgdGhpcy5mbnMucHVzaChtID0+IHsgbS5IREVMKGtleSwgZmllbGRzKTsgcmV0dXJuIG07IH0pO1xuICAgICAgfVxuICAgICAgenJlbXJhbmdlYnlzY29yZShrZXk6IHN0cmluZywgbWluU2NvcmU6IHN0cmluZywgaW5jbHVzaXZlTWF4U2NvcmU6IHN0cmluZyk6IHZvaWQge1xuICAgICAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgWlJFTVJOQUdFQllTQ09SRTogJHtrZXl9YCk7XG4gICAgICAgIHRoaXMuZm5zLnB1c2gobSA9PiB7IG0uWlJFTVJBTkdFQllTQ09SRShrZXksIG1pblNjb3JlLCBpbmNsdXNpdmVNYXhTY29yZSk7IHJldHVybiBtOyB9KTtcbiAgICAgIH1cbiAgICAgIHpyYW5nZWJ5c2NvcmUoa2V5OiBzdHJpbmcsIGluY2x1c2l2ZU1pblNjb3JlOiBzdHJpbmcsIGluY2x1c2l2ZU1heFNjb3JlOiBzdHJpbmcsIG9wdGlvbnM/OiB7IG9mZnNldDogbnVtYmVyLCBjb3VudDogbnVtYmVyIH0pOiB2b2lkIHtcbiAgICAgICAgLy9jb25zb2xlLmluZm8oYE1VTFRJIFJFRElTIFpSQU5HRUJZU0NPUkU6ICR7a2V5fWApO1xuICAgICAgICBpZiAob3B0aW9ucyA9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgICB0aGlzLmZucy5wdXNoKG0gPT4geyBtLlpSQU5HRUJZU0NPUkUoa2V5LCBpbmNsdXNpdmVNaW5TY29yZSwgaW5jbHVzaXZlTWF4U2NvcmUpOyByZXR1cm4gbTsgfSk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgdGhpcy5mbnMucHVzaChtID0+IHsgbS5aUkFOR0VCWVNDT1JFKGtleSwgaW5jbHVzaXZlTWluU2NvcmUsIGluY2x1c2l2ZU1heFNjb3JlLCAnTElNSVQnLCBvcHRpb25zLm9mZnNldCwgb3B0aW9ucy5jb3VudCk7IHJldHVybiBtOyB9KTtcbiAgICAgICAgfVxuICAgICAgfVxuICAgICAgenJldnJhbmdlYnlzY29yZShrZXk6IHN0cmluZywgaW5jbHVzaXZlTWluU2NvcmU6IHN0cmluZywgaW5jbHVzaXZlTWF4U2NvcmU6IHN0cmluZywgb3B0aW9ucz86IHsgb2Zmc2V0OiBudW1iZXIsIGNvdW50OiBudW1iZXIgfSk6IHZvaWQge1xuICAgICAgICAvL2NvbnNvbGUuaW5mbyhgTVVMVEkgUkVESVMgWlJFVlJBTkdFQllTQ09SRTogJHtrZXl9YCk7XG4gICAgICAgIGlmIChvcHRpb25zID09IHVuZGVmaW5lZCkge1xuICAgICAgICAgIHRoaXMuZm5zLnB1c2gobSA9PiB7IG0uWlJFVlJBTkdFQllTQ09SRShrZXksIGluY2x1c2l2ZU1heFNjb3JlLCBpbmNsdXNpdmVNaW5TY29yZSk7IHJldHVybiBtOyB9KTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICB0aGlzLmZucy5wdXNoKG0gPT4geyBtLlpSRVZSQU5HRUJZU0NPUkUoa2V5LCBpbmNsdXNpdmVNYXhTY29yZSwgaW5jbHVzaXZlTWluU2NvcmUsICdMSU1JVCcsIG9wdGlvbnMub2Zmc2V0LCBvcHRpb25zLmNvdW50KTsgcmV0dXJuIG07IH0pO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgICB6cmVtKGtleTogc3RyaW5nLCBtZW1iZXJzOiBzdHJpbmdbXSk6IHZvaWQge1xuICAgICAgICB0aGlzLmZucy5wdXNoKG0gPT4geyBtLlpSRU0oa2V5LCBtZW1iZXJzKTsgcmV0dXJuIG07IH0pO1xuICAgICAgfVxuICAgICAgZ2VvYWRkKGtleTogc3RyaW5nLCB2YWx1ZXM6IHsgbG9uOiBudW1iZXIsIGxhdDogbnVtYmVyLCB2YWx1ZTogc3RyaW5nIH1bXSk6IHZvaWQge1xuICAgICAgICBpZiAodmFsdWVzLmxlbmd0aCA9PSAwKSByZXR1cm47XG4gICAgICAgIGNvbnN0IHBhcmFtcyA9IFtdLmNvbmNhdC5hcHBseShbXSwgdmFsdWVzLm1hcCh2ID0+IFt2Lmxvbiwgdi5sYXQsIHYudmFsdWVdKSkgYXMgKHN0cmluZyB8IG51bWJlcilbXTtcbiAgICAgICAgdGhpcy5mbnMucHVzaChtID0+IHsgbS5HRU9BREQoa2V5LCBwYXJhbXMpOyByZXR1cm4gbTsgfSk7XG4gICAgICB9XG4gICAgICBnZW9yYWRpdXMoa2V5OiBzdHJpbmcsIGxvbjogbnVtYmVyLCBsYXQ6IG51bWJlciwgcmFkaXVzOiBudW1iZXIsIHVuaXQ6ICdtJyB8ICdrbScgfCAnZnQnIHwgJ21pJywgb3B0aW9ucz86IHsgY291bnQ6IG51bWJlciB9KTogdm9pZCB7XG4gICAgICAgIGlmIChvcHRpb25zID09IHVuZGVmaW5lZCkge1xuICAgICAgICAgIHRoaXMuZm5zLnB1c2gobSA9PiB7IG0uR0VPUkFESVVTKGtleSwgbG9uLCBsYXQsIHJhZGl1cywgdW5pdCk7IHJldHVybiBtOyB9KTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICB0aGlzLmZucy5wdXNoKG0gPT4geyBtLkdFT1JBRElVUyhrZXksIGxvbiwgbGF0LCByYWRpdXMsIHVuaXQsICdDT1VOVCcsIG9wdGlvbnMuY291bnQpOyByZXR1cm4gbTsgfSk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICAgIGFzeW5jIGV4ZWMoKTogUHJvbWlzZTxzdHJpbmdbXT4ge1xuICAgICAgICBsZXQgbXVsdGkgPSAoYXdhaXQgdGhhdC5lbnN1cmVDbGllbnQoKSkubXVsdGkoKTtcbiAgICAgICAgdGhpcy5mbnMuZm9yRWFjaChmbiA9PiB7IG11bHRpID0gZm4obXVsdGkpOyB9KTtcbiAgICAgICAgcmV0dXJuIG5ldyBQcm9taXNlPHN0cmluZ1tdPigocmVzb2x2ZSwgcmVqZWN0KSA9PiB7XG4gICAgICAgICAgbXVsdGkuZXhlYygoZXJyLCByZXBsaWVzKSA9PiB7XG4gICAgICAgICAgICBpZiAoZXJyKSB7XG4gICAgICAgICAgICAgIHJlamVjdChlcnIpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgcmVzb2x2ZShyZXBsaWVzLm1hcChyID0+IHIudG9TdHJpbmcoKSkpO1xuICAgICAgICAgICAgfVxuICAgICAgICAgIH0pO1xuICAgICAgICB9KTtcbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIG5ldyBNdWx0aUNhY2hlQ2xpZW50KCk7XG4gIH1cbn07XG4iXX0=