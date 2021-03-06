local json = require "json"


local button_name = "generate-data-raw-file"
local initialized = false

local function go()
    local count = game.entity_prototypes["DATA_RAW_COUNT"].order

    local data_chunks = {}
    for index = 1,count do
        table.insert(
            data_chunks,
            game.entity_prototypes["DATA_RAW"..index].order)
    end
    data_str = table.concat(data_chunks, "")
    local raw = loadstring(data_str)()

    itemTypes =
        { "item"
        , "fluid"
        , "ammo"
        , "mining-tool"
        , "car"
        , "tool"
        , "gun"
        , "module"
        , "capsule"
        , "repair-tool"
        , "armor"
        , "rail-planner"
        , "locomotive"
        , "fluid-wagon"
        , "cargo-wagon"
        , "artillery-wagon"
        }

    local itemIcons = {}
    for i = 1, #itemTypes do
        for k, item in pairs(raw[itemTypes[i]]) do
            local icons = item.icons
            if icons == nil then
                icons = {
                    {
                        icon = item.icon,
                    }
                }
            end
            local tmp = {
                name = item.name,
                icons = icons
            }
            table.insert(itemIcons, tmp)
        end
    end

    local data = {}
    data.raw = {}
    data.raw.itemIcons = itemIcons

    local recipeIcons = {}
    for k, recipe in pairs(raw.recipe) do
        local icons = recipe.icons
        if icons == nil and recipe.icon ~= nil then
            icons = {
                {
                    icon = recipe.icon,
                }
            }
        end
        local tmp = {
            name = recipe.name,
            icons = icons
        }
        table.insert(recipeIcons, tmp)
    end
    data.raw.recipeIcons = recipeIcons

    local inGameData = {}
    local recipes = {}
    for k, recipe in pairs(game.forces.player.recipes) do
        local tmp = {
            name = recipe.name,
            category = recipe.category,
            ingredients = recipe.ingredients,
            products = recipe.products,
            energy = recipe.energy
        }
        table.insert(recipes, tmp)
    end
    inGameData.inGameRecipes = recipes

    local craftingMachines = {}
    for k, entity in pairs(game.entity_prototypes) do
        local categories = entity.crafting_categories
        if categories ~= nil then
            local tmp = {
                name = k,
                categories = entity.crafting_categories,
                craftingSpeed = entity.crafting_speed,
                ingredientCount = entity.ingredient_count,
                moduleSlots = 0
            }
            local module_slots = entity.module_inventory_size
            if module_slots ~= nil then
                tmp.moduleSlots = module_slots
            else
                tmp.moduleSlots = 0
            end
            table.insert(craftingMachines, tmp)
        end
    end
    inGameData.inGameCraftingMachines = craftingMachines

    local modules = {}
    for k, module in pairs(game.item_prototypes) do
        local module_effects = module.module_effects
        if module_effects ~= nil then
            local tmp = {
                name = k,
                effects = module.module_effects,
                category = module.category,
                tier = module.tier,
                limitations = module.limitations
            }
            table.insert(modules, tmp)
        end
    end
    inGameData.inGameModules = modules
    data.inGameData = inGameData
    game.write_file("calculator-data.json", json.encode(data))
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
