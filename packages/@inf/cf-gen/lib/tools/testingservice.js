var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
var grant_1 = require("./grant");
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
;
exports.default = TestingService;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidGVzdGluZ3NlcnZpY2UuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi90b29scy90ZXN0aW5nc2VydmljZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7Ozs7O0FBQUEsaUNBQWdDO0FBR2hDO0lBQUE7SUFTQSxDQUFDO0lBTkMseUNBQWdCLEdBQWhCLFVBQWlCLElBQWtCLEVBQUUsR0FBVyxJQUFZLE9BQU8sR0FBRyxDQUFDLFdBQVcsRUFBRSxDQUFDLENBQUMsQ0FBQztJQUd2RiwyQ0FBa0IsR0FBbEIsVUFBbUIsSUFBa0IsRUFBRSxHQUFXLElBQVksT0FBTyxHQUFHLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQyxDQUFDO0lBRXpGLHdDQUFlLEdBQWYsVUFBZ0IsSUFBa0IsRUFBRSxHQUFXLElBQVksT0FBTyxHQUFHLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQyxDQUFDO0lBTHRGO1FBREMsYUFBSyxDQUFDLENBQUMsUUFBUSxDQUFDLENBQUM7MERBQ3FFO0lBR3ZGO1FBREMsYUFBSyxDQUFDLEVBQUUsQ0FBQzs0REFDK0U7SUFHM0YscUJBQUM7Q0FBQSxBQVRELElBU0M7QUFBQSxDQUFDO0FBRUYsa0JBQWUsY0FBYyxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgZ3JhbnQgfSBmcm9tICcuL2dyYW50JztcbmltcG9ydCB7IElVc2VyQ29udGV4dCB9IGZyb20gJy4uL3R5cGVzJztcblxuY2xhc3MgVGVzdGluZ1NlcnZpY2Uge1xuXG4gIEBncmFudChbJ2FkbWlucyddKVxuICBwcml2YXRlVXBwZXJDYXNlKHVzZXI6IElVc2VyQ29udGV4dCwgYXJnOiBzdHJpbmcpOiBzdHJpbmcgeyByZXR1cm4gYXJnLnRvVXBwZXJDYXNlKCk7IH1cblxuICBAZ3JhbnQoW10pXG4gIHByb3RlY3RlZFVwcGVyQ2FzZSh1c2VyOiBJVXNlckNvbnRleHQsIGFyZzogc3RyaW5nKTogc3RyaW5nIHsgcmV0dXJuIGFyZy50b1VwcGVyQ2FzZSgpOyB9XG5cbiAgcHVibGljVXBwZXJDYXNlKHVzZXI6IElVc2VyQ29udGV4dCwgYXJnOiBzdHJpbmcpOiBzdHJpbmcgeyByZXR1cm4gYXJnLnRvVXBwZXJDYXNlKCk7IH1cbn07XG5cbmV4cG9ydCBkZWZhdWx0IFRlc3RpbmdTZXJ2aWNlO1xuIl19