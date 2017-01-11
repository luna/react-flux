/* jshint sub:true */

function hsreact$mk_class(name, renderCb, checkState, releaseState, compProps, compState) {
    var cl = {
        'displayName': name,
        'componentWillReceiveProps': function() {
            h$release(this['props'].hs);
        },
        _updateAndReleaseState: function(s) {
            h$release(this['state'].hs);
            this['setState']({hs: s});
        },
        _updateState: function(s) {
            this['setState']({hs: s});
        },
        'componentWillUnmount': function() {
            this._currentCallbacks.map(h$release);
            h$release(this['props'].hs);
            if (releaseState) {
                h$release(this['state'].hs);
            }
        },
        'render': function() {
            var arg = {
                newCallbacks: [],
                elem:null
            };
            renderCb(this, arg);
            this._currentCallbacks.map(h$release);
            this._currentCallbacks = arg.newCallbacks;
            return arg.elem;
        },
        _currentCallbacks: []
    };


    if (checkState) {
        cl['shouldComponentUpdate'] = function(newProps, newState) {
            return !compProps(this['props'].hs, newProps.hs) || !compState(this['state'].hs, newState.hs);
        };
    } else {
        cl['shouldComponentUpdate'] = function(newProps, newState) {
            return !compProps(this['props'].hs, newProps.hs);
        };
    }

    if (typeof ReactIntl != "undefined") {
        cl['contextTypes'] = {
            'intl': ReactIntl['intlShape']
        };
    }

    return cl;
}

function hsreact$mk_ctrl_view(name, store, renderCb, compProps, compState) {
    var cl = hsreact$mk_class(name, renderCb, true, false, compProps, compState);
    cl['getInitialState'] = function() {
        return {hs: store.sdata};
    };
    cl['componentDidMount'] = function() {
        store.views.push(this._updateState);
    };
    cl['componentWillUnmount'] = function() {
        var idx = store.views.indexOf(this._updateState);
        if (idx >= 0) { store.views.splice(idx, 1); }
        this._currentCallbacks.map(h$release);
        h$release(this['props'].hs);
    };
    return React['createClass'](cl);
}

function hsreact$mk_view(name, renderCb, compProps) {
    return React['createClass'](hsreact$mk_class(name, renderCb, false, false, compProps));
}

function hsreact$mk_stateful_view(name, initialState, renderCb, compProps, compState) {
    var cl = hsreact$mk_class(name, renderCb, true, true, compProps, compState);
    cl['getInitialState'] = function() {
        return { hs: initialState };
    };
    return React['createClass'](cl);
}

function hsreact$mk_lifecycle_view(name, initialState, renderCb,
            willMountCb, didMountCb, willRecvPropsCb, willUpdateCb, didUpdateCb, willUnmountCb, compProps, compState) {
    var cl = hsreact$mk_class(name, renderCb, true, true, compProps, compState);

    cl['getInitialState'] = function() {
        return { hs: initialState };
    };

    if (willMountCb) {
        cl['componentWillMount'] = function() {
            willMountCb(this);
        };
    }

    if (didMountCb) {
        cl['componentDidMount'] = function() {
            didMountCb(this);
        };
    }

    if (willRecvPropsCb) {
        cl['componentWillReceiveProps'] = function(newProps) {
            try {
                willRecvPropsCb(this, newProps.hs);
            } finally {
                h$release(this['props'].hs);
            }
        };
    }

    if (willUpdateCb) {
        cl['componentWillUpdate'] = function(nextProps, nextState) {
            willUpdateCb(this, {'props': nextProps, 'state': nextState});
        };
    }

    if (didUpdateCb) {
        cl['componentDidUpdate'] = function(oldProps, oldState) {
            didUpdateCb(this, {'props': oldProps, 'state': oldState});
        };
    }

    if (willUnmountCb) {
        cl['componentWillUnmount'] = function() {
            try {
                willUnmountCb(this);
            } finally {
                this._currentCallbacks.map(h$release);
                h$release(this['props'].hs);
                h$release(this['state'].hs);
            }
        };
    }

    return React['createClass'](cl);
}

//React 0.14 introduced React.Children.toArray.  Also, to be able to run template haskell splices,
//we need to defend againsg React not being defined.
var hsreact$children_to_array = typeof React !== "object" ? null : (React['Children']['toArray'] ? React['Children']['toArray'] :
    (function (children) {
        var ret = [];
        React['Children']['forEach'](children, function(x) {
            ret.push(x);
        });
        return ret;
    }));
