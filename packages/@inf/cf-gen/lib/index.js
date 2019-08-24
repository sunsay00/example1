Object.defineProperty(exports, "__esModule", { value: true });
var rdsstore_1 = require("./back/api/src/api/stores/rdsstore");
var cachestore_1 = require("./back/api/src/api/stores/cachestore");
var mapper_1 = require("./back/api/src/api/mapper");
var resolver_1 = require("./tools/resolver");
var fix = require("./back/api/src/api/__tests__/fixtures");
exports.fixtures = fix;
exports.createCachedResolver = function (stage, notifications, cache, db, services) {
    return new resolver_1.Resolver(stage, mapper_1.createServiceMapper(notifications, new cachestore_1.default(new rdsstore_1.default(stage, db), cache), services));
};
exports.createResolver = function (stage, notifications, db, services) {
    return new resolver_1.Resolver(stage, mapper_1.createServiceMapper(notifications, new rdsstore_1.default(stage, db), services));
};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi9pbmRleC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsK0RBQTBEO0FBQzFELG1FQUE4RDtBQUU5RCxvREFBZ0Y7QUFDaEYsNkNBQTRDO0FBQzVDLDJEQUE2RDtBQUloRCxRQUFBLFFBQVEsR0FBRyxHQUFHLENBQUM7QUFFZixRQUFBLG9CQUFvQixHQUFHLFVBQ2xDLEtBQWEsRUFDYixhQUFzQyxFQUN0QyxLQUFtQixFQUNuQixFQUFhLEVBQ2IsUUFBMkI7SUFFM0IsT0FBQSxJQUFJLG1CQUFRLENBQUMsS0FBSyxFQUFFLDRCQUFtQixDQUFDLGFBQWEsRUFBRSxJQUFJLG9CQUFVLENBQUMsSUFBSSxrQkFBUSxDQUFDLEtBQUssRUFBRSxFQUFFLENBQUMsRUFBRSxLQUFLLENBQUMsRUFBRSxRQUFRLENBQUMsQ0FBQztBQUFqSCxDQUFpSCxDQUFDO0FBRXZHLFFBQUEsY0FBYyxHQUFHLFVBQzVCLEtBQWEsRUFDYixhQUFzQyxFQUN0QyxFQUFhLEVBQ2IsUUFBMkI7SUFFM0IsT0FBQSxJQUFJLG1CQUFRLENBQUMsS0FBSyxFQUFFLDRCQUFtQixDQUFDLGFBQWEsRUFBRSxJQUFJLGtCQUFRLENBQUMsS0FBSyxFQUFFLEVBQUUsQ0FBQyxFQUFFLFFBQVEsQ0FBQyxDQUFDO0FBQTFGLENBQTBGLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgUkRTU3RvcmUgZnJvbSAnLi9iYWNrL2FwaS9zcmMvYXBpL3N0b3Jlcy9yZHNzdG9yZSc7XG5pbXBvcnQgQ2FjaGVTdG9yZSBmcm9tICcuL2JhY2svYXBpL3NyYy9hcGkvc3RvcmVzL2NhY2hlc3RvcmUnO1xuaW1wb3J0IHsgSVVzZXJDb250ZXh0LCBJTm90aWZpY2F0aW9uTWFuYWdlciwgSURCQ2xpZW50LCBJQ2FjaGVDbGllbnQgfSBmcm9tICcuL3R5cGVzJztcbmltcG9ydCB7IGNyZWF0ZVNlcnZpY2VNYXBwZXIsIE1hcHBlZFNlcnZpY2VzIH0gZnJvbSAnLi9iYWNrL2FwaS9zcmMvYXBpL21hcHBlcic7XG5pbXBvcnQgeyBSZXNvbHZlciB9IGZyb20gJy4vdG9vbHMvcmVzb2x2ZXInO1xuaW1wb3J0ICogYXMgZml4IGZyb20gJy4vYmFjay9hcGkvc3JjL2FwaS9fX3Rlc3RzX18vZml4dHVyZXMnO1xuXG5leHBvcnQgKiBmcm9tICcuL3R5cGVzJztcbmV4cG9ydCAqIGZyb20gJy4vYmFjay9hcGkvc3JjL3R5cGVzL3NlcnZpY2VpbnRlcmZhY2VzJztcbmV4cG9ydCBjb25zdCBmaXh0dXJlcyA9IGZpeDtcblxuZXhwb3J0IGNvbnN0IGNyZWF0ZUNhY2hlZFJlc29sdmVyID0gPEMgZXh0ZW5kcyBJVXNlckNvbnRleHQ+KFxuICBzdGFnZTogc3RyaW5nLFxuICBub3RpZmljYXRpb25zOiBJTm90aWZpY2F0aW9uTWFuYWdlcjxDPixcbiAgY2FjaGU6IElDYWNoZUNsaWVudCxcbiAgZGI6IElEQkNsaWVudCxcbiAgc2VydmljZXM6IE1hcHBlZFNlcnZpY2VzPEM+LFxuKSA9PlxuICBuZXcgUmVzb2x2ZXIoc3RhZ2UsIGNyZWF0ZVNlcnZpY2VNYXBwZXIobm90aWZpY2F0aW9ucywgbmV3IENhY2hlU3RvcmUobmV3IFJEU1N0b3JlKHN0YWdlLCBkYiksIGNhY2hlKSwgc2VydmljZXMpKTtcblxuZXhwb3J0IGNvbnN0IGNyZWF0ZVJlc29sdmVyID0gPEMgZXh0ZW5kcyBJVXNlckNvbnRleHQ+KFxuICBzdGFnZTogc3RyaW5nLFxuICBub3RpZmljYXRpb25zOiBJTm90aWZpY2F0aW9uTWFuYWdlcjxDPixcbiAgZGI6IElEQkNsaWVudCxcbiAgc2VydmljZXM6IE1hcHBlZFNlcnZpY2VzPEM+LFxuKSA9PlxuICBuZXcgUmVzb2x2ZXIoc3RhZ2UsIGNyZWF0ZVNlcnZpY2VNYXBwZXIobm90aWZpY2F0aW9ucywgbmV3IFJEU1N0b3JlKHN0YWdlLCBkYiksIHNlcnZpY2VzKSk7Il19