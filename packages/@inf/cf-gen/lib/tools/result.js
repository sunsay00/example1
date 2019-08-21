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
exports.fn = function (f) { return new Promise(function (y, n) { return __awaiter(_this, void 0, void 0, function () {
    var x, _a, _b, err_1;
    return __generator(this, function (_c) {
        switch (_c.label) {
            case 0:
                _c.trys.push([0, 3, , 4]);
                x = f();
                if (!(typeof x == 'object')) return [3 /*break*/, 2];
                _a = y;
                _b = exports.ok;
                return [4 /*yield*/, x];
            case 1:
                _a.apply(void 0, [_b.apply(void 0, [_c.sent()])]);
                _c.label = 2;
            case 2: return [3 /*break*/, 4];
            case 3:
                err_1 = _c.sent();
                //if (err instanceof _Err) { throw err; } else
                y(exports.er(err_1));
                return [3 /*break*/, 4];
            case 4: return [2 /*return*/];
        }
    });
}); }); };
function to(promise) {
    if (promise && promise.then != undefined) {
        return promise.then(function (k) { return exports.ok(k); }).catch(function (err) { return exports.er(err); });
    }
    else {
        try {
            return exports.ok(promise);
        }
        catch (err) {
            return exports.er(err);
        }
    }
}
exports.to = to;
exports.ok = function (v) { return ({ __result: true, ok: v }); };
exports.er = function (v) { return ({ __result: false, ok: null, err: v }); };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicmVzdWx0LmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vdG9vbHMvcmVzdWx0LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7O0FBQUEsaUJBMEJBOztBQXhCYSxRQUFBLEVBQUUsR0FBRyxVQUFJLENBQTBCLElBQXlCLE9BQUEsSUFBSSxPQUFPLENBQVksVUFBTyxDQUFDLEVBQUUsQ0FBQzs7Ozs7O2dCQUVqRyxDQUFDLEdBQUcsQ0FBQyxFQUFFLENBQUM7cUJBQ1YsQ0FBQSxPQUFPLENBQUMsSUFBSSxRQUFRLENBQUEsRUFBcEIsd0JBQW9CO2dCQUFJLEtBQUEsQ0FBQyxDQUFBO2dCQUFDLEtBQUEsVUFBRSxDQUFBO2dCQUFDLHFCQUFNLENBQUMsRUFBQTs7Z0JBQVosa0JBQUUsa0JBQUcsU0FBTyxFQUFDLEVBQUMsQ0FBQzs7Ozs7Z0JBRTNDLDhDQUE4QztnQkFDOUMsQ0FBQyxDQUFDLFVBQUUsQ0FBQyxLQUFHLENBQUMsQ0FBQyxDQUFDOzs7OztLQUVkLENBQUMsRUFSdUUsQ0FRdkUsQ0FBQztBQUdILFNBQWdCLEVBQUUsQ0FBSSxPQUF1QjtJQUMzQyxJQUFJLE9BQU8sSUFBSyxPQUFlLENBQUMsSUFBSSxJQUFJLFNBQVMsRUFBRTtRQUNqRCxPQUFRLE9BQXNCLENBQUMsSUFBSSxDQUFDLFVBQUEsQ0FBQyxJQUFJLE9BQUEsVUFBRSxDQUFDLENBQUMsQ0FBQyxFQUFMLENBQUssQ0FBQyxDQUFDLEtBQUssQ0FBQyxVQUFBLEdBQUcsSUFBSSxPQUFBLFVBQUUsQ0FBQyxHQUFHLENBQUMsRUFBUCxDQUFPLENBQUMsQ0FBQztLQUN2RTtTQUFNO1FBQ0wsSUFBSTtZQUNGLE9BQU8sVUFBRSxDQUFDLE9BQVksQ0FBQyxDQUFDO1NBQ3pCO1FBQUMsT0FBTyxHQUFHLEVBQUU7WUFDWixPQUFPLFVBQUUsQ0FBQyxHQUFHLENBQUMsQ0FBQztTQUNoQjtLQUNGO0FBQ0gsQ0FBQztBQVZELGdCQVVDO0FBQ1ksUUFBQSxFQUFFLEdBQUcsVUFBZSxDQUFJLElBQWdCLE9BQUEsQ0FBQyxFQUFFLFFBQVEsRUFBRSxJQUFJLEVBQUUsRUFBRSxFQUFFLENBQUMsRUFBRSxDQUFDLEVBQTNCLENBQTJCLENBQUM7QUFDcEUsUUFBQSxFQUFFLEdBQUcsVUFBZSxDQUFNLElBQWdCLE9BQUEsQ0FBQyxFQUFFLFFBQVEsRUFBRSxLQUFLLEVBQUUsRUFBRSxFQUFFLElBQVcsRUFBRSxHQUFHLEVBQUUsQ0FBQyxFQUFFLENBQUMsRUFBOUMsQ0FBOEMsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbImV4cG9ydCB0eXBlIFJlc3VsdDxUPiA9IHsgX19yZXN1bHQ6IGJvb2xlYW4sIGVycj86IGFueSwgb2s6IFQgfVxuXG5leHBvcnQgY29uc3QgZm4gPSA8Uj4oZjogKCkgPT4gdm9pZCB8IFByb21pc2U8Uj4pOiBQcm9taXNlPFJlc3VsdDxSPj4gPT4gbmV3IFByb21pc2U8UmVzdWx0PFI+Pihhc3luYyAoeSwgbikgPT4ge1xuICB0cnkge1xuICAgIGNvbnN0IHggPSBmKCk7XG4gICAgaWYgKHR5cGVvZiB4ID09ICdvYmplY3QnKSB7IHkob2soYXdhaXQgeCkpOyB9XG4gIH0gY2F0Y2ggKGVycikge1xuICAgIC8vaWYgKGVyciBpbnN0YW5jZW9mIF9FcnIpIHsgdGhyb3cgZXJyOyB9IGVsc2VcbiAgICB5KGVyKGVycikpO1xuICB9XG59KTtcblxuZXhwb3J0IGZ1bmN0aW9uIHRvPFQsIFUgPSBuZXZlcj4ocHJvbWlzZTogUHJvbWlzZTxUPik6IFByb21pc2U8UmVzdWx0PFQ+PjtcbmV4cG9ydCBmdW5jdGlvbiB0bzxUPihwcm9taXNlOiBUIHwgUHJvbWlzZTxUPik6IFJlc3VsdDxUPiB8IFByb21pc2U8UmVzdWx0PFQ+PiB7XG4gIGlmIChwcm9taXNlICYmIChwcm9taXNlIGFzIGFueSkudGhlbiAhPSB1bmRlZmluZWQpIHtcbiAgICByZXR1cm4gKHByb21pc2UgYXMgUHJvbWlzZTxUPikudGhlbihrID0+IG9rKGspKS5jYXRjaChlcnIgPT4gZXIoZXJyKSk7XG4gIH0gZWxzZSB7XG4gICAgdHJ5IHtcbiAgICAgIHJldHVybiBvayhwcm9taXNlIGFzIFQpO1xuICAgIH0gY2F0Y2ggKGVycikge1xuICAgICAgcmV0dXJuIGVyKGVycik7XG4gICAgfVxuICB9XG59XG5leHBvcnQgY29uc3Qgb2sgPSA8VCwgVSA9IG5ldmVyPih2OiBUKTogUmVzdWx0PFQ+ID0+ICh7IF9fcmVzdWx0OiB0cnVlLCBvazogdiB9KTtcbmV4cG9ydCBjb25zdCBlciA9IDxULCBVID0gbmV2ZXI+KHY6IGFueSk6IFJlc3VsdDxUPiA9PiAoeyBfX3Jlc3VsdDogZmFsc2UsIG9rOiBudWxsIGFzIGFueSwgZXJyOiB2IH0pO1xuIl19