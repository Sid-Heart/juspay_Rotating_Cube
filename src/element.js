"use strict";
exports.body = function() {
    return jQuery(document.body);
};

exports.on = function(evt) {
    return function(act) {
        return function(ob) {
            return function() {
                ob.on(evt, function(e) {
                    act(e)(jQuery(this))();
                });
            };
        };
    };
};


exports.getPageX = function(e) {
    return function() {
        return e.pageX;
    };
};

exports.getElementById = function(name) {
    return function() {
        return jQuery("#" + name);
    };
};

exports.getPageY = function(e) {
    return function() {
        return e.pageY;
    };
};
