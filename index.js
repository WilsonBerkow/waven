(function () {
    "use strict";
    var waven = document.getElementById('waven');

    var mkZeroPulser = function (timeZero) {
        return {
            timeZero: timeZero,
            start: 0,
            duration: 0,
            amplitude: 0,
            phaseShift: 0,
            period: 1000
        };
    };

    var initPorts = {
        leftPulserSpecs: [mkZeroPulser(Date.now())]
    };

    var wavenApp = Elm.embed(Elm.Main, waven, initPorts);

    var leftPulserForms = document.getElementById('left-pulser-specs');
    var mkElem = (function () {
        var proto = {
            toString: function () {
                var self = this;
                var html = '<' + self.tag;
                Object.keys(self.attrs).forEach(function (name) {
                    html += " " + name + "=" + self.attrs[name];
                });
                html += '>\n';
                html += self.content.join("\n");
                html += '</' + self.name + '>';
                return html
            },
            attr: function (name, val) {
                this.attrs[name] = val;
            },
            append: function (elem) {
                this.content.push(elem);
                return elem;
            },
            make: function () {
                var self = this;
                var el = document.createElement(self.tag);
                Object.keys(self.attrs).forEach(function (key) {
                    el.setAttribute(key, self.attrs[key]);
                });
                Object.keys(self.on).forEach(function (on) {
                    el.addEventListener(on, self.on[on]);
                });
                self.content.forEach(function (item) {
                    if (typeof item === "string") {
                        el.appendChild(document.createTextNode(item));
                    } else {
                        el.appendChild(item.make());
                    }
                });
                return el;
            }
        };
        return function (tag, attrs, content, on) {
            var o = Object.create(proto);
            o.tag = tag;
            o.attrs = attrs || [];
            o.content = content || [];
            o.on = on || {};
            return o;
        };
    }());

    var mkSpecLine = function (fieldName, key, defaultValue) {
        var line = mkElem("label", {class: "pulser-spec-field"});
        line.append(fieldName + ": ");
        line.append(mkElem("input", {
            type: "text",
            means: key,
            value: defaultValue
        }));
        return line;
    };

    var mkPulserForm = function () {
        var form = mkElem("div", {class: "pulser-spec"});
        form.append(mkElem("div", {class: "pulser-spec-title"}, ["Pulser"]));
        form.append(mkSpecLine("Start Time (ms)", "start", 100));
        form.append(mkSpecLine("Amplitude", "amplitude", 20));
        form.append(mkSpecLine("Duration", "duration", 400));
        form.append(mkSpecLine("Phase Shift", "phaseShift", 0));
        form.append(mkSpecLine("Period", "period", 600));
        form.append(mkElem("button", {}, ["Remove this pulser"], {
            click: function (event) {
                event.target.parentElement.remove();
            }
        }));
        return form;
    };

    var newLeftPulser = document.getElementById('new-left-pulser');
    var leftPulserConfirm = document.getElementById('use-pulsers');
    //var newRightPulser = document.getElementById('new-right-pulser');

    newLeftPulser.addEventListener('click', function (event) {
        var specForm = mkPulserForm();
        leftPulserForms.appendChild(specForm.make());
    });
    leftPulserConfirm.addEventListener('click', function () {
        var relativeStartTime = performance.now();
        var pulsers = [];
        var specs = [].slice.apply(leftPulserForms.children);
        specs.forEach(function (spec) {
            var fields = [].slice.apply(spec.children);
            var specData = {};
            fields.forEach(function (fieldLine) {
                var input = fieldLine.querySelector('input');
                if (input === null) {
                    return;
                }
                var key = input.getAttribute('means');
                var value = input.value;
                specData[key] = +value;
            });
            specData.timeZero = relativeStartTime;
            pulsers.push(specData);
            console.log("Spec: ", specData);
        });
        wavenApp.ports.leftPulserSpecs.send(pulsers);
    });
}());
