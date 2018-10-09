local button_name = "generate-data-raw-file"
local initialized = false

local function toJson(data)
    if type(data) ~= "table" then
        error("Data is not a table!!!")
    end

    local jsonTable = {}
    for k,v in pairs(data) do
        local jsonFeild = "\"" .. k .. "\": "
        if type(v) == "table" then
            jsonFeild = jsonFeild .. toJson(v)
        else
            jsonFeild = jsonFeild .. "\"" .. tostring(v) .. "\""
        end
        table.insert(jsonTable, jsonFeild)
    end
    return "{" .. table.concat(jsonTable, ",\n") .. "}"
end

local function go()
    local count = game.entity_prototypes["DATA_RAW_COUNT"].order

    local data_chunks = {}
    for index = 1,count do
        table.insert(
            data_chunks,
            game.entity_prototypes["DATA_RAW"..index].order)
    end
    data_str = table.concat(data_chunks, "")
    loadstring(data_str)
    data = {raw = loadstring(data_str)}
    game.write_file("data-raw.json", toJson(data.raw()))
end

local function on_gui_click(event)
    if event.element.name == button_name then
        go()
    end
end


script.on_event(defines.events.on_gui_click, on_gui_click)
script.on_event(defines.events.on_tick, function(event)
    if initialized == false then
        for _, player in pairs(game.players) do
            initialized = true
            player.gui.top.add{
                type="button",
                caption="generate data-raw.json file",
                name=button_name}
        end
    end
end)
