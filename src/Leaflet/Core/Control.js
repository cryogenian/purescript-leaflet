var L = require("leaflet");

exports.layers_ = function(baseLayers, overlays, options) {
    return function() {
        return L.control.layers(baseLayers, overlays, options);
    };
};

exports.addTo_ = function(map, control) {
    return function() {
        return control.addTo(map);
    };
};

exports.remove_ = function(control) {
    return function() {
        return control.remove();
    };
};

exports.control_ = function() {
    return new L.Control();
};

exports.initControl_ = function(onAdd, onRemove, control, map) {
    return function() {
        control.onAdd = function(map) {
            return onAdd(this, map)();
        };
        control.onRemove = function(map) {
            onRemove(this, map)();
        };
    };
};
