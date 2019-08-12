const path = require('path');
let _currentPath;
const basename = (x) => { let i = x.lastIndexOf('/'); return x.substr(i < 0 ? 0 : i + 1); };
const dirname = (x) => { let i = x.lastIndexOf('/'); return x.substr(0, i < 0 ? 0 : i); };
const relative = (x) => '.' + x.substr(__dirname.length);
module.exports = function (params) {
    const types = params.types;
    const void0Expression = types.unaryExpression('void', types.numericLiteral(0), true);
    return {
        visitor: {
            Program: function (prog, state) {
                const filename = path.resolve(state.file.opts.filename);
                _currentPath = relative(filename);
            },
            CallExpression(path) {
                if (_currentPath == undefined)
                    return;
                const node = path.node;
                const first = node.arguments.length > 0 ? node.arguments[0] : undefined;
                if (first != undefined && first.value != undefined && first.type == 'StringLiteral') {
                    if (first.value == '[INFO]' ||
                        first.value == '[LOG]' ||
                        first.value == '[WARN]' ||
                        first.value == '[ERROR]') {
                        //console.log('\n\n+++', node.arguments[0], '\n\n---');
                        return;
                    }
                }
                if (types.isCallExpression(node)) {
                    if (types.isMemberExpression(node.callee)) {
                        const mem = node.callee;
                        if (types.isIdentifier(mem.object, { name: 'console' })) {
                            if (types.isIdentifier(mem.property, { name: 'info' })) {
                                node.arguments.unshift(types.stringLiteral(`[${mem.property.name.toUpperCase()}]`));
                            }
                            else if (types.isIdentifier(mem.property, { name: 'log' }) ||
                                types.isIdentifier(mem.property, { name: 'warn' }) ||
                                types.isIdentifier(mem.property, { name: 'error' })) {
                                const location = path.node.loc;
                                const str = `(${dirname(_currentPath)}/${basename(_currentPath)}:${location && location.start.line})`;
                                node.arguments.unshift(types.stringLiteral(`[${mem.property.name.toUpperCase()}]`));
                                node.arguments.push(types.stringLiteral(str));
                            }
                        }
                    }
                    else if (types.isIdentifier(node.callee, { name: 'ASSERT' })) {
                        const location = path.node.loc;
                        const str = `(${dirname(_currentPath)}/${basename(_currentPath)}:${location && location.start.line})`;
                        node.arguments.push(types.stringLiteral(str));
                    }
                    else if (types.isIdentifier(node.callee, { name: 'ERROR' })) {
                        const location = path.node.loc;
                        const str = `(${dirname(_currentPath)}/${basename(_currentPath)}:${location && location.start.line})`;
                        node.arguments.push(types.stringLiteral(str));
                    }
                }
            },
            ReferencedIdentifier(path) {
                if (path.node.name == '__line') {
                    const location = path.node.loc;
                    path.replaceWith(location && location.start.line ?
                        types.numericLiteral(location.start.line) :
                        void0Expression);
                }
                else if (path.node.name == '__filename') {
                    path.replaceWith(types.stringLiteral(_currentPath == undefined ? '???' : basename(_currentPath)));
                }
                else if (path.node.name == '__dirname') {
                    path.replaceWith(types.stringLiteral(_currentPath == undefined ? '???' : dirname(_currentPath)));
                }
            }
        }
    };
};
//# sourceMappingURL=transform.js.map