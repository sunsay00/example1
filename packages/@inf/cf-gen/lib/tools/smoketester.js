var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
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
var _this = this;
Object.defineProperty(exports, "__esModule", { value: true });
exports.runCacheStoreSmokeTests = function (store, client) { return __awaiter(_this, void 0, void 0, function () {
    var value, _a;
    return __generator(this, function (_b) {
        switch (_b.label) {
            case 0: return [4 /*yield*/, client.del(['check:test-key'])];
            case 1:
                _b.sent();
                return [4 /*yield*/, client.get('check:test-key')];
            case 2:
                value = _b.sent();
                if (value != undefined)
                    throw new Error('redis get key smoketest failed');
                return [4 /*yield*/, client.set('check:test-key', '123')];
            case 3:
                _b.sent();
                _a = '123';
                return [4 /*yield*/, client.get('check:test-key')];
            case 4:
                if (_a != (_b.sent()))
                    throw new Error('redis get key smoketest failed (2)');
                return [4 /*yield*/, store.runSmokeTests()];
            case 5: return [2 /*return*/, _b.sent()];
        }
    });
}); };
exports.runRDSDBStoreSmokeTests = function (client) { return __awaiter(_this, void 0, void 0, function () {
    var ret;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, client.query('select 1 + 1 as sum;', [])];
            case 1:
                ret = _a.sent();
                if (ret.rowCount != 1 && ret.rows[0]['sum'] != 2)
                    throw new Error('rds smoketest failed');
                return [2 /*return*/, 'success'];
        }
    });
}); };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic21va2V0ZXN0ZXIuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi90b29scy9zbW9rZXRlc3Rlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7OztBQUFBLGlCQWdCRTs7QUFkVyxRQUFBLHVCQUF1QixHQUFHLFVBQStCLEtBQWdCLEVBQUUsTUFBb0I7Ozs7b0JBQzFHLHFCQUFNLE1BQU0sQ0FBQyxHQUFHLENBQUMsQ0FBQyxnQkFBZ0IsQ0FBQyxDQUFDLEVBQUE7O2dCQUFwQyxTQUFvQyxDQUFDO2dCQUN2QixxQkFBTSxNQUFNLENBQUMsR0FBRyxDQUFDLGdCQUFnQixDQUFDLEVBQUE7O2dCQUExQyxLQUFLLEdBQUcsU0FBa0M7Z0JBQ2hELElBQUksS0FBSyxJQUFJLFNBQVM7b0JBQUUsTUFBTSxJQUFJLEtBQUssQ0FBQyxnQ0FBZ0MsQ0FBQyxDQUFDO2dCQUMxRSxxQkFBTSxNQUFNLENBQUMsR0FBRyxDQUFDLGdCQUFnQixFQUFFLEtBQUssQ0FBQyxFQUFBOztnQkFBekMsU0FBeUMsQ0FBQztnQkFDdEMsS0FBQSxLQUFLLENBQUE7Z0JBQUkscUJBQU0sTUFBTSxDQUFDLEdBQUcsQ0FBQyxnQkFBZ0IsQ0FBQyxFQUFBOztnQkFBL0MsSUFBSSxPQUFTLFNBQWtDLENBQUE7b0JBQUUsTUFBTSxJQUFJLEtBQUssQ0FBQyxvQ0FBb0MsQ0FBQyxDQUFDO2dCQUVoRyxxQkFBTSxLQUFLLENBQUMsYUFBYSxFQUFFLEVBQUE7b0JBQWxDLHNCQUFPLFNBQTJCLEVBQUM7OztLQUNwQyxDQUFDO0FBRVcsUUFBQSx1QkFBdUIsR0FBRyxVQUFPLE1BQWlCOzs7O29CQUNqRCxxQkFBTSxNQUFNLENBQUMsS0FBSyxDQUFDLHNCQUFzQixFQUFFLEVBQUUsQ0FBQyxFQUFBOztnQkFBcEQsR0FBRyxHQUFHLFNBQThDO2dCQUMxRCxJQUFJLEdBQUcsQ0FBQyxRQUFRLElBQUksQ0FBQyxJQUFJLEdBQUcsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLENBQUMsS0FBSyxDQUFDLElBQUksQ0FBQztvQkFBRSxNQUFNLElBQUksS0FBSyxDQUFDLHNCQUFzQixDQUFDLENBQUM7Z0JBQzFGLHNCQUFPLFNBQVMsRUFBQzs7O0tBQ2xCLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyBJU3RvcmUsIElVc2VyQ29udGV4dCwgSUNhY2hlQ2xpZW50LCBJREJDbGllbnQgfSBmcm9tICcuLi90eXBlcyc7XG5cbmV4cG9ydCBjb25zdCBydW5DYWNoZVN0b3JlU21va2VUZXN0cyA9IGFzeW5jIDxDIGV4dGVuZHMgSVVzZXJDb250ZXh0PihzdG9yZTogSVN0b3JlPEM+LCBjbGllbnQ6IElDYWNoZUNsaWVudCkgPT4ge1xuICBhd2FpdCBjbGllbnQuZGVsKFsnY2hlY2s6dGVzdC1rZXknXSk7XG4gIGNvbnN0IHZhbHVlID0gYXdhaXQgY2xpZW50LmdldCgnY2hlY2s6dGVzdC1rZXknKTtcbiAgaWYgKHZhbHVlICE9IHVuZGVmaW5lZCkgdGhyb3cgbmV3IEVycm9yKCdyZWRpcyBnZXQga2V5IHNtb2tldGVzdCBmYWlsZWQnKTtcbiAgYXdhaXQgY2xpZW50LnNldCgnY2hlY2s6dGVzdC1rZXknLCAnMTIzJyk7XG4gIGlmICgnMTIzJyAhPSBhd2FpdCBjbGllbnQuZ2V0KCdjaGVjazp0ZXN0LWtleScpKSB0aHJvdyBuZXcgRXJyb3IoJ3JlZGlzIGdldCBrZXkgc21va2V0ZXN0IGZhaWxlZCAoMiknKTtcblxuICByZXR1cm4gYXdhaXQgc3RvcmUucnVuU21va2VUZXN0cygpO1xufTtcblxuZXhwb3J0IGNvbnN0IHJ1blJEU0RCU3RvcmVTbW9rZVRlc3RzID0gYXN5bmMgKGNsaWVudDogSURCQ2xpZW50KSA9PiB7XG4gIGNvbnN0IHJldCA9IGF3YWl0IGNsaWVudC5xdWVyeSgnc2VsZWN0IDEgKyAxIGFzIHN1bTsnLCBbXSk7XG4gIGlmIChyZXQucm93Q291bnQgIT0gMSAmJiByZXQucm93c1swXVsnc3VtJ10gIT0gMikgdGhyb3cgbmV3IEVycm9yKCdyZHMgc21va2V0ZXN0IGZhaWxlZCcpO1xuICByZXR1cm4gJ3N1Y2Nlc3MnO1xufTsiXX0=