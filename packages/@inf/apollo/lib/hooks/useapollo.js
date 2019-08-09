var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
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
var _this = this;
Object.defineProperty(exports, "__esModule", { value: true });
var React = require("react");
var react_native_1 = require("react-native");
var graphql_tag_1 = require("graphql-tag");
var react_apollo_hooks_1 = require("react-apollo-hooks");
var apollo_client_1 = require("apollo-client");
var apollo_cache_inmemory_1 = require("apollo-cache-inmemory");
var apollo_cache_persist_1 = require("apollo-cache-persist");
var apollo_link_http_1 = require("apollo-link-http");
var apollo_link_ws_1 = require("apollo-link-ws");
var apollo_link_1 = require("apollo-link");
var apollo_link_error_1 = require("apollo-link-error");
var apollo_utilities_1 = require("apollo-utilities");
var subscriptions_transport_ws_1 = require("subscriptions-transport-ws");
var apollo_link_context_1 = require("apollo-link-context");
var createCache = function () { return __awaiter(_this, void 0, void 0, function () {
    var cache;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                cache = new apollo_cache_inmemory_1.InMemoryCache();
                return [4 /*yield*/, apollo_cache_persist_1.persistCache({ cache: cache, storage: react_native_1.AsyncStorage })];
            case 1:
                _a.sent();
                return [2 /*return*/, cache];
        }
    });
}); };
var concatWebSocket = function (link, websocketEndpoint) {
    if (!websocketEndpoint)
        return link;
    var wsLink = new apollo_link_ws_1.WebSocketLink(new subscriptions_transport_ws_1.SubscriptionClient(websocketEndpoint, { reconnect: true, lazy: true }, undefined, []));
    wsLink.subscriptionClient.maxConnectTimeGenerator.setMin(3000);
    wsLink.subscriptionClient.maxConnectTimeGenerator.duration = function () { return wsLink.subscriptionClient.maxConnectTimeGenerator.max; };
    return apollo_link_1.split(function (op) {
        var def = apollo_utilities_1.getMainDefinition(op.query);
        return def.kind === 'OperationDefinition' && def.operation === 'subscription';
    }, wsLink, link);
};
var useAuthLink = function (authorization) {
    var _a = React.useState(apollo_link_context_1.setContext(function (_, ctx) { return (__assign({}, ctx, { headers: __assign({}, ctx.headers, { authorization: authorization }) })); })), authLink = _a[0], setAuthLink = _a[1];
    React.useEffect(function () {
        setAuthLink(apollo_link_context_1.setContext(function (_, ctx) { return (__assign({}, ctx, { headers: __assign({}, ctx.headers, { authorization: authorization }) })); }));
    }, [authorization]);
    return authLink;
};
var ApolloContext = React.createContext(undefined);
exports.useApollo = function () {
    var ctx = React.useContext(ApolloContext);
    if (!ctx)
        throw new Error('invalid apollo context');
    return ctx;
};
exports.ApolloProvider = function (props) {
    var _a = React.useState(undefined), client = _a[0], setClient = _a[1];
    var authLink = useAuthLink(props.authorization);
    React.useEffect(function () {
        createCache().then(function (cache) {
            var httpLink = new apollo_link_http_1.HttpLink({ uri: props.graphqlEndpoint });
            setClient(new apollo_client_1.ApolloClient({
                link: apollo_link_1.ApolloLink.from([
                    apollo_link_error_1.onError(function (_a) {
                        var graphQLErrors = _a.graphQLErrors, networkError = _a.networkError;
                        if (graphQLErrors)
                            graphQLErrors.map(function (_a) {
                                var message = _a.message, path = _a.path;
                                return console.error("[GraphQL error]: Message: " + message + ", Path: " + path);
                            });
                        if (networkError)
                            console.error("[Network error]: " + networkError);
                        debugger;
                    }),
                    concatWebSocket(authLink.concat(httpLink), props.websocketEndpoint),
                ]),
                cache: cache,
                resolvers: {
                    Video: {
                        logs: function (video, _args, _a) {
                            var cache = _a.cache, getCacheKey = _a.getCacheKey;
                            var id = getCacheKey({ __typename: 'Video', id: video.id });
                            var fragment = graphql_tag_1.default(templateObject_1 || (templateObject_1 = __makeTemplateObject(["fragment video on Video { logs @client }"], ["fragment video on Video { logs @client }"])));
                            var prev = cache.readFragment({ fragment: fragment, id: id });
                            if (!prev)
                                return [];
                            return prev.logs;
                        },
                    }
                }
            }));
        }).catch(console.error);
    }, [authLink, props.graphqlEndpoint, props.websocketEndpoint]);
    if (!client)
        return null;
    return (React.createElement(ApolloContext.Provider, { value: client },
        React.createElement(react_apollo_hooks_1.ApolloProvider, { client: client }, props.children)));
};
var templateObject_1;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidXNlYXBvbGxvLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vc3JjL2hvb2tzL3VzZWFwb2xsby50c3giXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7QUFBQSxpQkEwR0M7O0FBMUdELDZCQUErQjtBQUMvQiw2Q0FBNEM7QUFDNUMsMkNBQThCO0FBQzlCLHlEQUE4RDtBQUM5RCwrQ0FBNkM7QUFDN0MsK0RBQXNEO0FBQ3RELDZEQUFvRDtBQUNwRCxxREFBNEM7QUFDNUMsaURBQStDO0FBQy9DLDJDQUFnRDtBQUNoRCx1REFBNEM7QUFDNUMscURBQXFEO0FBQ3JELHlFQUFnRTtBQUNoRSwyREFBaUQ7QUFFakQsSUFBTSxXQUFXLEdBQUc7Ozs7O2dCQUNaLEtBQUssR0FBRyxJQUFJLHFDQUFhLEVBQUUsQ0FBQztnQkFDbEMscUJBQU0sbUNBQVksQ0FBQyxFQUFFLEtBQUssT0FBQSxFQUFFLE9BQU8sRUFBRSwyQkFBbUIsRUFBRSxDQUFDLEVBQUE7O2dCQUEzRCxTQUEyRCxDQUFDO2dCQUM1RCxzQkFBTyxLQUFLLEVBQUM7OztLQUNkLENBQUE7QUFFRCxJQUFNLGVBQWUsR0FBRyxVQUFDLElBQWdCLEVBQUUsaUJBQXFDO0lBQzlFLElBQUksQ0FBQyxpQkFBaUI7UUFBRSxPQUFPLElBQUksQ0FBQztJQUVwQyxJQUFNLE1BQU0sR0FBRyxJQUFJLDhCQUFhLENBQUMsSUFBSSwrQ0FBa0IsQ0FDckQsaUJBQWlCLEVBQ2pCLEVBQUUsU0FBUyxFQUFFLElBQUksRUFBRSxJQUFJLEVBQUUsSUFBSSxFQUFFLEVBQy9CLFNBQVMsRUFDVCxFQUFFLENBQ0gsQ0FBQyxDQUFDO0lBQ0YsTUFBYyxDQUFDLGtCQUFrQixDQUFDLHVCQUF1QixDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsQ0FBQztJQUN2RSxNQUFjLENBQUMsa0JBQWtCLENBQUMsdUJBQXVCLENBQUMsUUFBUSxH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QyxDQUFDOzRCQUM5RCxJQUFNLFFBQVEsR0FBRyxxQkFBRyw2R0FBQSwwQ0FBMEMsSUFBQSxDQUFDOzRCQUMvRCxJQUFNLElBQUksR0FBRyxLQUFLLENBQUMsWUFBWSxDQUFDLEVBQUUsUUFBUSxVQUFBLEVBQUUsRUFBRSxJQUFBLEVBQUUsQ0FBQyxDQUFDOzRCQUNsRCxJQUFJLENBQUMsSUFBSTtnQ0FBRSxPQUFPLEVBQUUsQ0FBQzs0QkFDckIsT0FBTyxJQUFJLENBQUMsSUFBSSxDQUFDO3dCQUNuQixDQUFDO3FCQUNGO2lCQUNGO2FBQ0YsQ0FBQyxDQUFDLENBQUM7UUFDTixDQUFDLENBQUMsQ0FBQyxLQUFLLENBQUMsT0FBTyxDQUFDLEtBQUssQ0FBQyxDQUFDO0lBQzFCLENBQUMsRUFBRSxDQUFDLFFBQVEsRUFBRSxLQUFLLENBQUMsZUFBZSxFQUFFLEtBQUssQ0FBQyxpQkFBaUIsQ0FBQyxDQUFDLENBQUM7SUFFL0QsSUFBSSxDQUFDLE1BQU07UUFBRSxPQUFPLElBQUksQ0FBQztJQUV6QixPQUFPLENBQ0wsb0JBQUMsYUFBYSxDQUFDLFFBQVEsSUFBQyxLQUFLLEVBQUUsTUFBTTtRQUNuQyxvQkFBQyxtQ0FBTSxJQUFDLE1BQU0sRUFBRSxNQUFNLElBQUcsS0FBSyxDQUFDLFFBQVEsQ0FBVSxDQUMxQixDQUMxQixDQUFDO0FBQ0osQ0FBQyxDQUFBIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0ICogYXMgUmVhY3QgZnJvbSAncmVhY3QnO1xuaW1wb3J0IHsgQXN5bmNTdG9yYWdlIH0gZnJvbSAncmVhY3QtbmF0aXZlJztcbmltcG9ydCBncWwgZnJvbSAnZ3JhcGhxbC10YWcnO1xuaW1wb3J0IHsgQXBvbGxvUHJvdmlkZXIgYXMgQXBvbGxvIH0gZnJvbSAncmVhY3QtYXBvbGxvLWhvb2tzJztcbmltcG9ydCB7IEFwb2xsb0NsaWVudCB9IGZyb20gJ2Fwb2xsby1jbGllbnQnO1xuaW1wb3J0IHsgSW5NZW1vcnlDYWNoZSB9IGZyb20gJ2Fwb2xsby1jYWNoZS1pbm1lbW9yeSc7XG5pbXBvcnQgeyBwZXJzaXN0Q2FjaGUgfSBmcm9tICdhcG9sbG8tY2FjaGUtcGVyc2lzdCc7XG5pbXBvcnQgeyBIdHRwTGluayB9IGZyb20gJ2Fwb2xsby1saW5rLWh0dHAnO1xuaW1wb3J0IHsgV2ViU29ja2V0TGluayB9IGZyb20gJ2Fwb2xsby1saW5rLXdzJztcbmltcG9ydCB7IEFwb2xsb0xpbmssIHNwbGl0IH0gZnJvbSAnYXBvbGxvLWxpbmsnO1xuaW1wb3J0IHsgb25FcnJvciB9IGZyb20gJ2Fwb2xsby1saW5rLWVycm9yJztcbmltcG9ydCB7IGdldE1haW5EZWZpbml0aW9uIH0gZnJvbSAnYXBvbGxvLXV0aWxpdGllcyc7XG5pbXBvcnQgeyBTdWJzY3JpcHRpb25DbGllbnQgfSBmcm9tICdzdWJzY3JpcHRpb25zLXRyYW5zcG9ydC13cyc7XG5pbXBvcnQgeyBzZXRDb250ZXh0IH0gZnJvbSAnYXBvbGxvLWxpbmstY29udGV4dCc7XG5cbmNvbnN0IGNyZWF0ZUNhY2hlID0gYXN5bmMgKCkgPT4ge1xuICBjb25zdCBjYWNoZSA9IG5ldyBJbk1lbW9yeUNhY2hlKCk7XG4gIGF3YWl0IHBlcnNpc3RDYWNoZSh7IGNhY2hlLCBzdG9yYWdlOiBBc3luY1N0b3JhZ2UgYXMgYW55IH0pO1xuICByZXR1cm4gY2FjaGU7XG59XG5cbmNvbnN0IGNvbmNhdFdlYlNvY2tldCA9IChsaW5rOiBBcG9sbG9MaW5rLCB3ZWJzb2NrZXRFbmRwb2ludDogc3RyaW5nIHwgdW5kZWZpbmVkKSA9PiB7XG4gIGlmICghd2Vic29ja2V0RW5kcG9pbnQpIHJldHVybiBsaW5rO1xuXG4gIGNvbnN0IHdzTGluayA9IG5ldyBXZWJTb2NrZXRMaW5rKG5ldyBTdWJzY3JpcHRpb25DbGllbnQoXG4gICAgd2Vic29ja2V0RW5kcG9pbnQsXG4gICAgeyByZWNvbm5lY3Q6IHRydWUsIGxhenk6IHRydWUgfSxcbiAgICB1bmRlZmluZWQsXG4gICAgW11cbiAgKSk7XG4gICh3c0xpbmsgYXMgYW55KS5zdWJzY3JpcHRpb25DbGllbnQubWF4Q29ubmVjdFRpbWVHZW5lcmF0b3Iuc2V0TWluKDMwMDApO1xuICAod3NMaW5rIGFzIGFueSkuc3Vic2NyaXB0aW9uQ2xpZW50Lm1heENvbm5lY3RUaW1lR2VuZXJhdG9yLmR1cmF0aW9uID0gKCkgPT4gKHdzTGluayBhcyBhbnkpLnN1YnNjcmlwdGlvbkNsaWVudC5tYXhDb25uZWN0VGltZUdlbmVyYXRvci5tYXg7XG5cbiAgcmV0dXJuIHNwbGl0KG9wID0+IHtcbiAgICBjb25zdCBkZWYgPSBnZXRNYWluRGVmaW5pdGlvbihvcC5xdWVyeSk7XG4gICAgcmV0dXJuIGRlZi5raW5kID09PSAnT3BlcmF0aW9uRGVmaW5pdGlvbicgJiYgZGVmLm9wZXJhdGlvbiA9PT0gJ3N1YnNjcmlwdGlvbic7XG4gIH0sIHdzTGluaywgbGluayk7XG59XG5cbmNvbnN0IHVzZUF1dGhMaW5rID0gKGF1dGhvcml6YXRpb246IHN0cmluZykgPT4ge1xuXG4gIGNvbnN0IFthdXRoTGluaywgc2V0QXV0aExpbmtdID0gUmVhY3QudXNlU3RhdGUoc2V0Q29udGV4dCgoXywgY3R4KSA9PiAoeyAuLi5jdHgsIGhlYWRlcnM6IHsgLi4uY3R4LmhlYWRlcnMsIGF1dGhvcml6YXRpb24gfSB9KSkpO1xuXG4gIFJlYWN0LnVzZUVmZmVjdCgoKSA9PiB7XG4gICAgc2V0QXV0aExpbmsoc2V0Q29udGV4dCgoXywgY3R4KSA9PiAoeyAuLi5jdHgsIGhlYWRlcnM6IHsgLi4uY3R4LmhlYWRlcnMsIGF1dGhvcml6YXRpb24gfSB9KSkpO1xuICB9LCBbYXV0aG9yaXphdGlvbl0pO1xuXG4gIHJldHVybiBhdXRoTGluaztcbn1cblxuY29uc3QgQXBvbGxvQ29udGV4dCA9IFJlYWN0LmNyZWF0ZUNvbnRleHQ8QXBvbGxvQ2xpZW50PHVua25vd24+IHwgdW5kZWZpbmVkPih1bmRlZmluZWQpO1xuXG5leHBvcnQgY29uc3QgdXNlQXBvbGxvID0gKCkgPT4ge1xuICBjb25zdCBjdHggPSBSZWFjdC51c2VDb250ZXh0KEFwb2xsb0NvbnRleHQpO1xuICBpZiAoIWN0eCkgdGhyb3cgbmV3IEVycm9yKCdpbnZhbGlkIGFwb2xsbyBjb250ZXh0Jyk7XG4gIHJldHVybiBjdHg7XG59XG5cbmV4cG9ydCBjb25zdCBBcG9sbG9Qcm92aWRlciA9IChwcm9wczoge1xuICBhdXRob3JpemF0aW9uOiBzdHJpbmcsXG4gIHdlYnNvY2tldEVuZHBvaW50OiBzdHJpbmcgfCB1bmRlZmluZWQsXG4gIGdyYXBocWxFbmRwb2ludDogc3RyaW5nLFxuICBjaGlsZHJlbj86IFJlYWN0LlJlYWN0Tm9kZVxufSkgPT4ge1xuICBjb25zdCBbY2xpZW50LCBzZXRDbGllbnRdID0gUmVhY3QudXNlU3RhdGU8QXBvbGxvQ2xpZW50PHVua25vd24+IHwgdW5kZWZpbmVkPih1bmRlZmluZWQpO1xuXG4gIGNvbnN0IGF1dGhMaW5rID0gdXNlQXV0aExpbmsocHJvcHMuYXV0aG9yaXphdGlvbik7XG5cbiAgUmVhY3QudXNlRWZmZWN0KCgpID0+IHtcbiAgICBjcmVhdGVDYWNoZSgpLnRoZW4oY2FjaGUgPT4ge1xuICAgICAgY29uc3QgaHR0cExpbmsgPSBuZXcgSHR0cExpbmsoeyB1cmk6IHByb3BzLmdyYXBocWxFbmRwb2ludCB9KTtcbiAgICAgIHNldENsaWVudChuZXcgQXBvbGxvQ2xpZW50KHtcbiAgICAgICAgbGluazogQXBvbGxvTGluay5mcm9tKFtcbiAgICAgICAgICBvbkVycm9yKCh7IGdyYXBoUUxFcnJvcnMsIG5ldHdvcmtFcnJvciB9KSA9PiB7XG4gICAgICAgICAgICBpZiAoZ3JhcGhRTEVycm9ycylcbiAgICAgICAgICAgICAgZ3JhcGhRTEVycm9ycy5tYXAoKHsgbWVzc2FnZSwgcGF0aCB9KSA9PlxuICAgICAgICAgICAgICAgIGNvbnNvbGUuZXJyb3IoYFtHcmFwaFFMIGVycm9yXTogTWVzc2FnZTogJHttZXNzYWdlfSwgUGF0aDogJHtwYXRofWApKTtcbiAgICAgICAgICAgIGlmIChuZXR3b3JrRXJyb3IpXG4gICAgICAgICAgICAgIGNvbnNvbGUuZXJyb3IoYFtOZXR3b3JrIGVycm9yXTogJHtuZXR3b3JrRXJyb3J9YCk7XG4gICAgICAgICAgICBkZWJ1Z2dlcjtcbiAgICAgICAgICB9KSxcbiAgICAgICAgICBjb25jYXRXZWJTb2NrZXQoYXV0aExpbmsuY29uY2F0KGh0dHBMaW5rKSwgcHJvcHMud2Vic29ja2V0RW5kcG9pbnQpLFxuICAgICAgICBdKSxcbiAgICAgICAgY2FjaGUsXG4gICAgICAgIHJlc29sdmVyczoge1xuICAgICAgICAgIFZpZGVvOiB7XG4gICAgICAgICAgICBsb2dzOiAodmlkZW8sIF9hcmdzLCB7IGNhY2hlLCBnZXRDYWNoZUtleSB9KSA9PiB7XG4gICAgICAgICAgICAgIGNvbnN0IGlkID0gZ2V0Q2FjaGVLZXkoeyBfX3R5cGVuYW1lOiAnVmlkZW8nLCBpZDogdmlkZW8uaWQgfSk7XG4gICAgICAgICAgICAgIGNvbnN0IGZyYWdtZW50ID0gZ3FsYGZyYWdtZW50IHZpZGVvIG9uIFZpZGVvIHsgbG9ncyBAY2xpZW50IH1gO1xuICAgICAgICAgICAgICBjb25zdCBwcmV2ID0gY2FjaGUucmVhZEZyYWdtZW50KHsgZnJhZ21lbnQsIGlkIH0pO1xuICAgICAgICAgICAgICBpZiAoIXByZXYpIHJldHVybiBbXTtcbiAgICAgICAgICAgICAgcmV0dXJuIHByZXYubG9ncztcbiAgICAgICAgICAgIH0sXG4gICAgICAgICAgfVxuICAgICAgICB9XG4gICAgICB9KSk7XG4gICAgfSkuY2F0Y2goY29uc29sZS5lcnJvcik7XG4gIH0sIFthdXRoTGluaywgcHJvcHMuZ3JhcGhxbEVuZHBvaW50LCBwcm9wcy53ZWJzb2NrZXRFbmRwb2ludF0pO1xuXG4gIGlmICghY2xpZW50KSByZXR1cm4gbnVsbDtcblxuICByZXR1cm4gKFxuICAgIDxBcG9sbG9Db250ZXh0LlByb3ZpZGVyIHZhbHVlPXtjbGllbnR9PlxuICAgICAgPEFwb2xsbyBjbGllbnQ9e2NsaWVudH0+e3Byb3BzLmNoaWxkcmVufTwvQXBvbGxvPlxuICAgIDwvQXBvbGxvQ29udGV4dC5Qcm92aWRlcj5cbiAgKTtcbn0iXX0=