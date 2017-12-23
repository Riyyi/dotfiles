--makes mpv only stay on top while playing
--please note that this won't do anything if ontop is not enabled before pausing
 
local was_ontop = false
 
mp.observe_property("pause", "bool", function(name, value)
    local ontop = mp.get_property_native("ontop")
    if value == true then
        if ontop == true then
            mp.set_property_native("ontop", false)
            was_ontop = true
        end
    elseif value == false then
        if was_ontop and (ontop == false) then
            mp.set_property_native("ontop", true)
        end
        was_ontop = false
    end
end)