---------------------------------------------------
-- Effect class for the awesome window manager
-- A port of mootools Fx Library
---------------------------------------------------
-- Licensed under MIT License
--  * (c) 2010, Georg Nagel <g.schlmm@gmail.com>
---------------------------------------------------

Effect = {}
Effect.__index = Effect

function Effect.create(obj, options)

    local fx = {
        options = {
            fps = 40,
            duration = 0.5
        }
    }

    setmetatable(fx,Effect)

    fx._obj = obj

    if type(options) == 'table' then
        fx:setOptions(options)
    end

    fx._timer = timer({ timeout = 1/fx.options.fps })
    fx._timer:add_signal("timeout", function () fx:step() end)

    fx._is_func = (type(fx._obj) == 'function')
    fx._props = {}

    return fx

end

function Effect:setOptions (options)
    tmp = self.options
    for k,v in pairs(options) do self.options[k] = v end
end

function Effect:start (properties)
    self._time = 0
    o = self._obj
    if self._is_func then
        o = self._obj()
    end

    if (self._timer.started) then
        self._timer:stop()
    end

    for k,v in pairs(self._props) do
        if not properties[k] and properties[k] ~= 0  then
            properties[k] = v.to
        end
    end

    for k,v in pairs(properties) do
        if o[k] then
            self._props[k] = { from = o[k], to = v }
        end
    end
    self:validateTransition()
    self._timer:start()
end

function Effect:stop()
    self._timer:stop()
end


function Effect:step ()
    self._time = self._time + self._timer.timeout
    local newProps = {}
    if (self._time < self.options.duration) then
        for k,v in pairs(self._props) do
            local delta = self.transition(self._time / self.options.duration)
            newProps[k] = self:compute(v.from, v.to, delta)
        end
    else
        for k,v in pairs(self._props) do
            newProps[k] = v.to
            self._timer:stop()
        end
    end

    self:set(newProps)
end

function Effect:set (props)
    if self._is_func then
        self._obj(props);
    else
        for k,v in pairs(self._props) do
            self._obj[prop] = value
        end
    end
end

function Effect:compute (from, to, delta)
    return (to - from) * delta + from
end


function Effect:validateTransition ()
    local trans = self.options.transition;
    if type(trans) ~= 'function' then
        trans = Effect.Transitions.linear
    end
    self.transition = trans;
end


Effect.Transitions = {}
function Effect.Transitions.linear (zero)
    return zero
end

local transitions = {}

function transitions.Pow (p, x)
    if type(x) == 'table' and x[0] then
        x = x[0]
    else
        x = 6
    end
    return math.pow(p, x);
end

function transitions.Expo (p)
    return math.pow(2, 8 * (p - 1));
end

function transitions.Circ (p)
    return 1 - math.sin(math.acos(p));
end

function Effect.Transitions.Sine (p)
    return 1 - math.sin((1 - p) * math.pi / 2);
end

function transitions.Back (p, x)
    if type(x) == 'table' and x[0] then
        x = x[0]
    else
        x = 1.618
    end
    return math.pow(p, 2) * ((x + 1) * p - x);
end

function transitions.Bounce (p)
    if p < 1/2.75 then
        return 7.5625*p*p
    end
    if p < 2/2.75 then
        p = p - (1.5/2.75)
        return 7.5625*p*p + 0.75
    end
    if p < 2.5/2.75 then
        p = p - (2.25/2.75)
        return 7.5625*p*p + 0.9375
    end
    p = p - (2.625/2.75)
    return 7.5625*p*p + 0.984375
end

function transitions.Elastic (p, x)
    p = p - 1
    if type(x) == 'table' and x[0] then
        x = x[0]
    else
        x = 3
    end
    return match.pow(2, 10 * p) * math.cos(20 * p * math.pi * x / 3);
end


for i, name in pairs({'Quad', 'Cubic', 'Quart', 'Quint'}) do
    transitions[name] = function (p)
        return math.pow(p, i + 2)
    end
end

Effect.Transitions.easeIn = {}
Effect.Transitions.easeOut = {}
Effect.Transitions.easeInOut = {}

for name,transition in pairs(transitions) do
    Effect.Transitions.easeIn[name] = transition
    Effect.Transitions.easeOut[name] = function (p)
        return 1 - transitions[name](1 - p);
    end
    Effect.Transitions.easeInOut[name] = function (p)
        local x = p <= 0.5 and
            transitions[name](2 * p, params) / 2
        or
            (2 - transitions[name](2 * (1 - p))) / 2
        return x
    end
end
