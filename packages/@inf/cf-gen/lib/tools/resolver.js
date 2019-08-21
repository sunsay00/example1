var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
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
Object.defineProperty(exports, "__esModule", { value: true });
var graphql_1 = require("graphql");
var graphql_tools_1 = require("graphql-tools");
var typedefs_1 = require("../back/api/src/api/resolver/v1/typedefs");
var resolvers_1 = require("../back/api/src/api/resolver/v1/resolvers");
var grant_1 = require("../tools/grant");
var TestingService = /** @class */ (function () {
    function TestingService() {
    }
    TestingService.prototype.privateUpperCase = function (user, arg) { return arg.toUpperCase(); };
    TestingService.prototype.protectedUpperCase = function (user, arg) { return arg.toUpperCase(); };
    TestingService.prototype.publicUpperCase = function (user, arg) { return arg.toUpperCase(); };
    __decorate([
        grant_1.grant(['admins'])
    ], TestingService.prototype, "privateUpperCase", null);
    __decorate([
        grant_1.grant([])
    ], TestingService.prototype, "protectedUpperCase", null);
    return TestingService;
}());
var Resolver = /** @class */ (function () {
    function Resolver(stage, mapper) {
        var _this = this;
        this.resolve = function (headers, query, variables, user) { return __awaiter(_this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                return [2 /*return*/, graphql_1.graphql(this._schema, query, undefined, user, variables)];
            });
        }); };
        this._stage = stage;
        this._mapper = mapper;
        this._schema = graphql_tools_1.makeExecutableSchema({
            typeDefs: typedefs_1.default,
            resolvers: resolvers_1.default(this._stage, this._mapper, new TestingService())
        });
    }
    return Resolver;
}());
exports.Resolver = Resolver;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicmVzb2x2ZXIuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi90b29scy9yZXNvbHZlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7QUFBQSxtQ0FBaUQ7QUFDakQsK0NBQXFEO0FBQ3JELHFFQUFnRTtBQUNoRSx1RUFBa0U7QUFHbEUsd0NBQXVDO0FBRXZDO0lBQUE7SUFNQSxDQUFDO0lBSkMseUNBQWdCLEdBQWhCLFVBQWlCLElBQWtCLEVBQUUsR0FBVyxJQUFZLE9BQU8sR0FBRyxDQUFDLFdBQVcsRUFBRSxDQUFDLENBQUMsQ0FBQztJQUV2RiwyQ0FBa0IsR0FBbEIsVUFBbUIsSUFBa0IsRUFBRSxHQUFXLElBQVksT0FBTyxHQUFHLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQyxDQUFDO0lBQ3pGLHdDQUFlLEdBQWYsVUFBZ0IsSUFBa0IsRUFBRSxHQUFXLElBQVksT0FBTyxHQUFHLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQyxDQUFDO0lBSHRGO1FBREMsYUFBSyxDQUFDLENBQUMsUUFBUSxDQUFDLENBQUM7MERBQ3FFO0lBRXZGO1FBREMsYUFBSyxDQUFDLEVBQUUsQ0FBQzs0REFDK0U7SUFFM0YscUJBQUM7Q0FBQSxBQU5ELElBTUM7QUFFRDtJQUtFLGtCQUFZLEtBQWEsRUFBRSxNQUFpQjtRQUE1QyxpQkFPQztRQUVELFlBQU8sR0FBRyxVQUErQixPQUFnQyxFQUFFLEtBQWEsRUFBRSxTQUFvQixFQUFFLElBQU87O2dCQUNySCxzQkFBTyxpQkFBTyxDQUFDLElBQUksQ0FBQyxPQUFPLEVBQUUsS0FBSyxFQUFFLFNBQVMsRUFBRSxJQUFJLEVBQUUsU0FBUyxDQUFDLEVBQUM7O2FBQ2pFLENBQUE7UUFWQyxJQUFJLENBQUMsTUFBTSxHQUFHLEtBQUssQ0FBQztRQUNwQixJQUFJLENBQUMsT0FBTyxHQUFHLE1BQU0sQ0FBQztRQUN0QixJQUFJLENBQUMsT0FBTyxHQUFHLG9DQUFvQixDQUFDO1lBQ2xDLFFBQVEsb0JBQUE7WUFDUixTQUFTLEVBQUUsbUJBQVMsQ0FBQyxJQUFJLENBQUMsTUFBTSxFQUFFLElBQUksQ0FBQyxPQUFPLEVBQUUsSUFBSSxjQUFjLEVBQUUsQ0FBQztTQUN0RSxDQUFDLENBQUM7SUFDTCxDQUFDO0lBS0gsZUFBQztBQUFELENBQUMsQUFqQkQsSUFpQkM7QUFqQlksNEJBQVEiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyBncmFwaHFsLCBHcmFwaFFMU2NoZW1hIH0gZnJvbSAnZ3JhcGhxbCc7XG5pbXBvcnQgeyBtYWtlRXhlY3V0YWJsZVNjaGVtYSB9IGZyb20gJ2dyYXBocWwtdG9vbHMnO1xuaW1wb3J0IHR5cGVEZWZzIGZyb20gJy4uL2JhY2svYXBpL3NyYy9hcGkvcmVzb2x2ZXIvdjEvdHlwZWRlZnMnO1xuaW1wb3J0IHJlc29sdmVycyBmcm9tICcuLi9iYWNrL2FwaS9zcmMvYXBpL3Jlc29sdmVyL3YxL3Jlc29sdmVycyc7XG5pbXBvcnQgTWFwcGVyIGZyb20gJy4uL2JhY2svYXBpL3NyYy9hcGkvbWFwcGVyJztcbmltcG9ydCB7IElVc2VyQ29udGV4dCwgVmFyaWFibGVzIH0gZnJvbSAnLi4vdHlwZXMnO1xuaW1wb3J0IHsgZ3JhbnQgfSBmcm9tICcuLi90b29scy9ncmFudCc7XG5cbmNsYXNzIFRlc3RpbmdTZXJ2aWNlIHtcbiAgQGdyYW50KFsnYWRtaW5zJ10pXG4gIHByaXZhdGVVcHBlckNhc2UodXNlcjogSVVzZXJDb250ZXh0LCBhcmc6IHN0cmluZyk6IHN0cmluZyB7IHJldHVybiBhcmcudG9VcHBlckNhc2UoKTsgfVxuICBAZ3JhbnQoW10pXG4gIHByb3RlY3RlZFVwcGVyQ2FzZSh1c2VyOiBJVXNlckNvbnRleHQsIGFyZzogc3RyaW5nKTogc3RyaW5nIHsgcmV0dXJuIGFyZy50b1VwcGVyQ2FzZSgpOyB9XG4gIHB1YmxpY1VwcGVyQ2FzZSh1c2VyOiBJVXNlckNvbnRleHQsIGFyZzogc3RyaW5nKTogc3RyaW5nIHsgcmV0dXJuIGFyZy50b1VwcGVyQ2FzZSgpOyB9XG59XG5cbmV4cG9ydCBjbGFzcyBSZXNvbHZlcjxDIGV4dGVuZHMgSVVzZXJDb250ZXh0PiB7XG4gIHByaXZhdGUgX3N0YWdlOiBzdHJpbmc7XG4gIHByaXZhdGUgX21hcHBlcjogTWFwcGVyPEM+O1xuICBwcml2YXRlIF9zY2hlbWE6IEdyYXBoUUxTY2hlbWE7XG5cbiAgY29uc3RydWN0b3Ioc3RhZ2U6IHN0cmluZywgbWFwcGVyOiBNYXBwZXI8Qz4pIHtcbiAgICB0aGlzLl9zdGFnZSA9IHN0YWdlO1xuICAgIHRoaXMuX21hcHBlciA9IG1hcHBlcjtcbiAgICB0aGlzLl9zY2hlbWEgPSBtYWtlRXhlY3V0YWJsZVNjaGVtYSh7XG4gICAgICB0eXBlRGVmcyxcbiAgICAgIHJlc29sdmVyczogcmVzb2x2ZXJzKHRoaXMuX3N0YWdlLCB0aGlzLl9tYXBwZXIsIG5ldyBUZXN0aW5nU2VydmljZSgpKVxuICAgIH0pO1xuICB9XG5cbiAgcmVzb2x2ZSA9IGFzeW5jIDxDIGV4dGVuZHMgSVVzZXJDb250ZXh0PihoZWFkZXJzOiB7IFtfOiBzdHJpbmddOiBzdHJpbmcgfSwgcXVlcnk6IHN0cmluZywgdmFyaWFibGVzOiBWYXJpYWJsZXMsIHVzZXI6IEMpID0+IHtcbiAgICByZXR1cm4gZ3JhcGhxbCh0aGlzLl9zY2hlbWEsIHF1ZXJ5LCB1bmRlZmluZWQsIHVzZXIsIHZhcmlhYmxlcyk7XG4gIH1cbn0iXX0=