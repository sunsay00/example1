var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
var language_1 = require("graphql/language");
var graphql_1 = require("graphql");
var error_1 = require("graphql/error");
var GraphQLRegExp = /** @class */ (function (_super) {
    __extends(GraphQLRegExp, _super);
    function GraphQLRegExp(opts) {
        return _super.call(this, {
            name: opts.name,
            description: opts.description,
            serialize: function (value) { return value; },
            parseValue: function (value) {
                if (!opts.exp.test(value)) {
                    throw new Error(opts.err);
                }
                return value;
            },
            parseLiteral: function (ast) {
                if (ast.kind !== language_1.Kind.STRING) {
                    throw new error_1.GraphQLError("Query error: expected string, got " + ast.kind, [ast]);
                }
                if (!opts.exp.test(ast.value)) {
                    throw new error_1.GraphQLError(opts.err, [ast]);
                }
                return ast.value;
            }
        }) || this;
    }
    return GraphQLRegExp;
}(graphql_1.GraphQLScalarType));
exports.GraphQLRegExp = GraphQLRegExp;
exports.GraphQLEmail = new GraphQLRegExp({
    name: 'Email',
    exp: /^(([^<>()[\]\.,;:\s@\"]+(\.[^<>()[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()[\]\.,;:\s@\"]+\.)+[^<>()[\]\.,;:\s@\"]{2,})$/i,
    err: 'invalid email',
    description: 'an email type following RFC 5322'
});
exports.GraphQLDateTime = new graphql_1.GraphQLScalarType({
    name: 'DateTime',
    description: 'a datetime type of format YYYY-MM-DDTHH:MM:SS.SSSZ',
    serialize: function (val) {
        var value = typeof val == 'string' ? new Date(val) : val;
        if (!(value instanceof Date)) {
            throw new Error("CustomType serialize error: expected an instance of Date, recevied " + JSON.stringify(value) + " " + typeof value);
        }
        if (isNaN(value.getTime())) {
            throw new Error("CustomType serialize error: invalid Date, recevied " + value);
        }
        return value.toJSON();
    },
    parseValue: function (value) {
        var date = new Date(value);
        if (isNaN(date.getTime())) {
            throw new Error('CustomType parseValue error: invalid Date');
        }
        return date;
    },
    parseLiteral: function (ast) {
        if (ast.kind !== language_1.Kind.STRING) {
            throw new error_1.GraphQLError("CustomType parseLiteral error: expected a date string, got " + ast.kind, [ast]);
        }
        var result = new Date(ast.value);
        if (isNaN(result.getTime())) {
            throw new error_1.GraphQLError('CustomType parseLiteral error: invalid date', [ast]);
        }
        if (ast.value !== result.toJSON()) {
            throw new error_1.GraphQLError('CustomType parseLiteral error: Invalid date format, only accepts: YYYY-MM-DDTHH:MM:SS.SSSZ', [ast]);
        }
        return result;
    }
});
exports.GraphQLPoint = new graphql_1.GraphQLScalarType({
    name: 'Point',
    description: 'a longitude latitude coordinate pair',
    serialize: function (val) {
        if (typeof val == 'string') {
            try {
                return JSON.parse(val);
            }
            catch (err) {
                throw new Error('failed to parse json');
            }
        }
        else {
            return val;
        }
    },
    parseValue: function (value) {
        try {
            var _a = typeof value == 'string' ? JSON.parse(value) : value, lon = _a.lon, lat = _a.lat;
            if (typeof lon != 'number' || typeof lat != 'number') {
                throw new Error('CustomType parseValue error: invalid Point (1)');
            }
            var ret = { lon: lon, lat: lat };
            return ret;
        }
        catch (err) {
            throw new Error("CustomType parseValue error: invalid Point (2) " + err);
        }
    },
    parseLiteral: function (ast) {
        if (ast.kind !== language_1.Kind.STRING) {
            throw new error_1.GraphQLError("CustomType parseLiteral error: expected a point string, got " + ast.kind, [ast]);
        }
        try {
            var _a = JSON.parse(ast.value), lon = _a.lon, lat = _a.lat;
            if (typeof lon != 'number' || typeof lat != 'number') {
                throw new Error('CustomType parseLiteral value error: invalid Point (1)');
            }
            var ret = { lon: lon, lat: lat };
            return ret;
        }
        catch (err) {
            throw new Error("CustomType parseLiteral error: invalid Point (2) " + err);
        }
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY3VzdG9tdHlwZXMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi90b29scy9jdXN0b210eXBlcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7Ozs7Ozs7Ozs7OztBQUFBLDZDQUF3QztBQUN4QyxtQ0FBNEM7QUFDNUMsdUNBQTZDO0FBRzdDO0lBQW1DLGlDQUFpQjtJQUNsRCx1QkFBWSxJQUFzRTtlQUNoRixrQkFBTTtZQUNKLElBQUksRUFBRSxJQUFJLENBQUMsSUFBSTtZQUNmLFdBQVcsRUFBRSxJQUFJLENBQUMsV0FBVztZQUM3QixTQUFTLEVBQUUsVUFBQyxLQUFhLElBQUssT0FBQSxLQUFLLEVBQUwsQ0FBSztZQUNuQyxVQUFVLEVBQUUsVUFBQyxLQUFhO2dCQUN4QixJQUFJLENBQUMsSUFBSSxDQUFDLEdBQUcsQ0FBQyxJQUFJLENBQUMsS0FBSyxDQUFDLEVBQUU7b0JBQ3pCLE1BQU0sSUFBSSxLQUFLLENBQUMsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDO2lCQUMzQjtnQkFDRCxPQUFPLEtBQUssQ0FBQztZQUNmLENBQUM7WUFDRCxZQUFZLEVBQUUsVUFBQSxHQUFHO2dCQUNmLElBQUksR0FBRyxDQUFDLElBQUksS0FBSyxlQUFJLENBQUMsTUFBTSxFQUFFO29CQUM1QixNQUFNLElBQUksb0JBQVksQ0FBQyx1Q0FBcUMsR0FBRyxDQUFDLElBQU0sRUFBRSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUM7aUJBQ2hGO2dCQUNELElBQUksQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLElBQUksQ0FBQyxHQUFHLENBQUMsS0FBSyxDQUFDLEVBQUU7b0JBQzdCLE1BQU0sSUFBSSxvQkFBWSxDQUFDLElBQUksQ0FBQyxHQUFHLEVBQUUsQ0FBQyxHQUFHLENBQUMsQ0FBQyxDQUFDO2lCQUN6QztnQkFDRCxPQUFPLEdBQUcsQ0FBQyxLQUFLLENBQUM7WUFDbkIsQ0FBQztTQUNGLENBQUM7SUFDSixDQUFDO0lBQ0gsb0JBQUM7QUFBRCxD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QUFDO1lBQzFFLElBQUksT0FBTyxHQUFHLElBQUksUUFBUSxJQUFJLE9BQU8sR0FBRyxJQUFJLFFBQVEsRUFBRTtnQkFDcEQsTUFBTSxJQUFJLEtBQUssQ0FBQyxnREFBZ0QsQ0FBQyxDQUFDO2FBQ25FO1lBQ0QsSUFBTSxHQUFHLEdBQVUsRUFBRSxHQUFHLEtBQUEsRUFBRSxHQUFHLEtBQUEsRUFBRSxDQUFDO1lBQ2hDLE9BQU8sR0FBRyxDQUFDO1NBQ1o7UUFBQyxPQUFPLEdBQUcsRUFBRTtZQUNaLE1BQU0sSUFBSSxLQUFLLENBQUMsb0RBQWtELEdBQUssQ0FBQyxDQUFDO1NBQzFFO0lBQ0gsQ0FBQztJQUNELFlBQVksRUFBRSxVQUFBLEdBQUc7UUFDZixJQUFJLEdBQUcsQ0FBQyxJQUFJLEtBQUssZUFBSSxDQUFDLE1BQU0sRUFBRTtZQUM1QixNQUFNLElBQUksb0JBQVksQ0FBQyxpRUFBK0QsR0FBRyxDQUFDLElBQU0sRUFBRSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUM7U0FDMUc7UUFDRCxJQUFJO1lBQ0ksSUFBQSwwQkFBb0MsRUFBbEMsWUFBRyxFQUFFLFlBQTZCLENBQUM7WUFDM0MsSUFBSSxPQUFPLEdBQUcsSUFBSSxRQUFRLElBQUksT0FBTyxHQUFHLElBQUksUUFBUSxFQUFFO2dCQUNwRCxNQUFNLElBQUksS0FBSyxDQUFDLHdEQUF3RCxDQUFDLENBQUM7YUFDM0U7WUFDRCxJQUFNLEdBQUcsR0FBVSxFQUFFLEdBQUcsS0FBQSxFQUFFLEdBQUcsS0FBQSxFQUFFLENBQUM7WUFDaEMsT0FBTyxHQUFHLENBQUM7U0FDWjtRQUFDLE9BQU8sR0FBRyxFQUFFO1lBQ1osTUFBTSxJQUFJLEtBQUssQ0FBQyxzREFBb0QsR0FBSyxDQUFDLENBQUM7U0FDNUU7SUFDSCxDQUFDO0NBQ0YsQ0FBQyxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgS2luZCB9IGZyb20gJ2dyYXBocWwvbGFuZ3VhZ2UnO1xuaW1wb3J0IHsgR3JhcGhRTFNjYWxhclR5cGUgfSBmcm9tICdncmFwaHFsJztcbmltcG9ydCB7IEdyYXBoUUxFcnJvciB9IGZyb20gJ2dyYXBocWwvZXJyb3InO1xuaW1wb3J0IHsgUG9pbnQgfSBmcm9tICcuLi90eXBlcyc7XG5cbmV4cG9ydCBjbGFzcyBHcmFwaFFMUmVnRXhwIGV4dGVuZHMgR3JhcGhRTFNjYWxhclR5cGUge1xuICBjb25zdHJ1Y3RvcihvcHRzOiB7IG5hbWU6IHN0cmluZywgZXhwOiBSZWdFeHAsIGVycjogc3RyaW5nLCBkZXNjcmlwdGlvbj86IHN0cmluZyB9KSB7XG4gICAgc3VwZXIoe1xuICAgICAgbmFtZTogb3B0cy5uYW1lLFxuICAgICAgZGVzY3JpcHRpb246IG9wdHMuZGVzY3JpcHRpb24sXG4gICAgICBzZXJpYWxpemU6ICh2YWx1ZTogc3RyaW5nKSA9PiB2YWx1ZSxcbiAgICAgIHBhcnNlVmFsdWU6ICh2YWx1ZTogc3RyaW5nKSA9PiB7XG4gICAgICAgIGlmICghb3B0cy5leHAudGVzdCh2YWx1ZSkpIHtcbiAgICAgICAgICB0aHJvdyBuZXcgRXJyb3Iob3B0cy5lcnIpO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiB2YWx1ZTtcbiAgICAgIH0sXG4gICAgICBwYXJzZUxpdGVyYWw6IGFzdCA9PiB7XG4gICAgICAgIGlmIChhc3Qua2luZCAhPT0gS2luZC5TVFJJTkcpIHtcbiAgICAgICAgICB0aHJvdyBuZXcgR3JhcGhRTEVycm9yKGBRdWVyeSBlcnJvcjogZXhwZWN0ZWQgc3RyaW5nLCBnb3QgJHthc3Qua2luZH1gLCBbYXN0XSk7XG4gICAgICAgIH1cbiAgICAgICAgaWYgKCFvcHRzLmV4cC50ZXN0KGFzdC52YWx1ZSkpIHtcbiAgICAgICAgICB0aHJvdyBuZXcgR3JhcGhRTEVycm9yKG9wdHMuZXJyLCBbYXN0XSk7XG4gICAgICAgIH1cbiAgICAgICAgcmV0dXJuIGFzdC52YWx1ZTtcbiAgICAgIH1cbiAgICB9KTtcbiAgfVxufVxuXG5leHBvcnQgY29uc3QgR3JhcGhRTEVtYWlsID0gbmV3IEdyYXBoUUxSZWdFeHAoe1xuICBuYW1lOiAnRW1haWwnLFxuICBleHA6IC9eKChbXjw+KClbXFxdXFwuLDs6XFxzQFxcXCJdKyhcXC5bXjw+KClbXFxdXFwuLDs6XFxzQFxcXCJdKykqKXwoXFxcIi4rXFxcIikpQCgoW148PigpW1xcXVxcLiw7Olxcc0BcXFwiXStcXC4pK1tePD4oKVtcXF1cXC4sOzpcXHNAXFxcIl17Mix9KSQvaSxcbiAgZXJyOiAnaW52YWxpZCBlbWFpbCcsXG4gIGRlc2NyaXB0aW9uOiAnYW4gZW1haWwgdHlwZSBmb2xsb3dpbmcgUkZDIDUzMjInXG59KTtcblxuZXhwb3J0IGNvbnN0IEdyYXBoUUxEYXRlVGltZSA9IG5ldyBHcmFwaFFMU2NhbGFyVHlwZSh7XG4gIG5hbWU6ICdEYXRlVGltZScsXG4gIGRlc2NyaXB0aW9uOiAnYSBkYXRldGltZSB0eXBlIG9mIGZvcm1hdCBZWVlZLU1NLUREVEhIOk1NOlNTLlNTU1onLFxuICBzZXJpYWxpemU6ICh2YWw6IERhdGUgfCBzdHJpbmcpID0+IHtcbiAgICBjb25zdCB2YWx1ZSA9IHR5cGVvZiB2YWwgPT0gJ3N0cmluZycgPyBuZXcgRGF0ZSh2YWwpIDogdmFsO1xuICAgIGlmICghKHZhbHVlIGluc3RhbmNlb2YgRGF0ZSkpIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihgQ3VzdG9tVHlwZSBzZXJpYWxpemUgZXJyb3I6IGV4cGVjdGVkIGFuIGluc3RhbmNlIG9mIERhdGUsIHJlY2V2aWVkICR7SlNPTi5zdHJpbmdpZnkodmFsdWUpfSAke3R5cGVvZiB2YWx1ZX1gKTtcbiAgICB9XG4gICAgaWYgKGlzTmFOKHZhbHVlLmdldFRpbWUoKSkpIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihgQ3VzdG9tVHlwZSBzZXJpYWxpemUgZXJyb3I6IGludmFsaWQgRGF0ZSwgcmVjZXZpZWQgJHt2YWx1ZX1gKTtcbiAgICB9XG4gICAgcmV0dXJuIHZhbHVlLnRvSlNPTigpO1xuICB9LFxuICBwYXJzZVZhbHVlOiAodmFsdWU6IHN0cmluZykgPT4ge1xuICAgIGNvbnN0IGRhdGUgPSBuZXcgRGF0ZSh2YWx1ZSk7XG4gICAgaWYgKGlzTmFOKGRhdGUuZ2V0VGltZSgpKSkge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKCdDdXN0b21UeXBlIHBhcnNlVmFsdWUgZXJyb3I6IGludmFsaWQgRGF0ZScpO1xuICAgIH1cbiAgICByZXR1cm4gZGF0ZTtcbiAgfSxcbiAgcGFyc2VMaXRlcmFsOiBhc3QgPT4ge1xuICAgIGlmIChhc3Qua2luZCAhPT0gS2luZC5TVFJJTkcpIHtcbiAgICAgIHRocm93IG5ldyBHcmFwaFFMRXJyb3IoYEN1c3RvbVR5cGUgcGFyc2VMaXRlcmFsIGVycm9yOiBleHBlY3RlZCBhIGRhdGUgc3RyaW5nLCBnb3QgJHthc3Qua2luZH1gLCBbYXN0XSk7XG4gICAgfVxuICAgIGNvbnN0IHJlc3VsdCA9IG5ldyBEYXRlKGFzdC52YWx1ZSk7XG4gICAgaWYgKGlzTmFOKHJlc3VsdC5nZXRUaW1lKCkpKSB7XG4gICAgICB0aHJvdyBuZXcgR3JhcGhRTEVycm9yKCdDdXN0b21UeXBlIHBhcnNlTGl0ZXJhbCBlcnJvcjogaW52YWxpZCBkYXRlJywgW2FzdF0pXG4gICAgfVxuICAgIGlmIChhc3QudmFsdWUgIT09IHJlc3VsdC50b0pTT04oKSkge1xuICAgICAgdGhyb3cgbmV3IEdyYXBoUUxFcnJvcignQ3VzdG9tVHlwZSBwYXJzZUxpdGVyYWwgZXJyb3I6IEludmFsaWQgZGF0ZSBmb3JtYXQsIG9ubHkgYWNjZXB0czogWVlZWS1NTS1ERFRISDpNTTpTUy5TU1NaJywgW2FzdF0pXG4gICAgfVxuICAgIHJldHVybiByZXN1bHQ7XG4gIH1cbn0pO1xuXG5leHBvcnQgY29uc3QgR3JhcGhRTFBvaW50ID0gbmV3IEdyYXBoUUxTY2FsYXJUeXBlKHtcbiAgbmFtZTogJ1BvaW50JyxcbiAgZGVzY3JpcHRpb246ICdhIGxvbmdpdHVkZSBsYXRpdHVkZSBjb29yZGluYXRlIHBhaXInLFxuICBzZXJpYWxpemU6ICh2YWw6IFBvaW50IHwgc3RyaW5nKSA9PiB7XG4gICAgaWYgKHR5cGVvZiB2YWwgPT0gJ3N0cmluZycpIHtcbiAgICAgIHRyeSB7XG4gICAgICAgIHJldHVybiBKU09OLnBhcnNlKHZhbCk7XG4gICAgICB9IGNhdGNoIChlcnIpIHtcbiAgICAgICAgdGhyb3cgbmV3IEVycm9yKCdmYWlsZWQgdG8gcGFyc2UganNvbicpO1xuICAgICAgfVxuICAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4gdmFsO1xuICAgIH1cbiAgfSxcbiAgcGFyc2VWYWx1ZTogKHZhbHVlOiBQb2ludCB8IHN0cmluZykgPT4ge1xuICAgIHRyeSB7XG4gICAgICBjb25zdCB7IGxvbiwgbGF0IH0gPSB0eXBlb2YgdmFsdWUgPT0gJ3N0cmluZycgPyBKU09OLnBhcnNlKHZhbHVlKSA6IHZhbHVlO1xuICAgICAgaWYgKHR5cGVvZiBsb24gIT0gJ251bWJlcicgfHwgdHlwZW9mIGxhdCAhPSAnbnVtYmVyJykge1xuICAgICAgICB0aHJvdyBuZXcgRXJyb3IoJ0N1c3RvbVR5cGUgcGFyc2VWYWx1ZSBlcnJvcjogaW52YWxpZCBQb2ludCAoMSknKTtcbiAgICAgIH1cbiAgICAgIGNvbnN0IHJldDogUG9pbnQgPSB7IGxvbiwgbGF0IH07XG4gICAgICByZXR1cm4gcmV0O1xuICAgIH0gY2F0Y2ggKGVycikge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKGBDdXN0b21UeXBlIHBhcnNlVmFsdWUgZXJyb3I6IGludmFsaWQgUG9pbnQgKDIpICR7ZXJyfWApO1xuICAgIH1cbiAgfSxcbiAgcGFyc2VMaXRlcmFsOiBhc3QgPT4ge1xuICAgIGlmIChhc3Qua2luZCAhPT0gS2luZC5TVFJJTkcpIHtcbiAgICAgIHRocm93IG5ldyBHcmFwaFFMRXJyb3IoYEN1c3RvbVR5cGUgcGFyc2VMaXRlcmFsIGVycm9yOiBleHBlY3RlZCBhIHBvaW50IHN0cmluZywgZ290ICR7YXN0LmtpbmR9YCwgW2FzdF0pO1xuICAgIH1cbiAgICB0cnkge1xuICAgICAgY29uc3QgeyBsb24sIGxhdCB9ID0gSlNPTi5wYXJzZShhc3QudmFsdWUpO1xuICAgICAgaWYgKHR5cGVvZiBsb24gIT0gJ251bWJlcicgfHwgdHlwZW9mIGxhdCAhPSAnbnVtYmVyJykge1xuICAgICAgICB0aHJvdyBuZXcgRXJyb3IoJ0N1c3RvbVR5cGUgcGFyc2VMaXRlcmFsIHZhbHVlIGVycm9yOiBpbnZhbGlkIFBvaW50ICgxKScpO1xuICAgICAgfVxuICAgICAgY29uc3QgcmV0OiBQb2ludCA9IHsgbG9uLCBsYXQgfTtcbiAgICAgIHJldHVybiByZXQ7XG4gICAgfSBjYXRjaCAoZXJyKSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoYEN1c3RvbVR5cGUgcGFyc2VMaXRlcmFsIGVycm9yOiBpbnZhbGlkIFBvaW50ICgyKSAke2Vycn1gKTtcbiAgICB9XG4gIH1cbn0pO1xuIl19