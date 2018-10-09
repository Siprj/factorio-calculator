function dataChunk(str, index)
    data:extend({{
        type = "flying-text",
        name = "DATA_RAW"..tostring(index),
        time_to_live = 0,
        speed = 1,
        order = str
    }})
end

function doStuff()
    local rawData = serpent.dump(data.raw)
    local chunkList = {}
    local chunkSize = 200
    for position = 1, #rawData, chunkSize do
        table.insert(chunkList, rawData:sub(position, position + chunkSize - 1))
    end

    local maxCount = 0
    for index, chunk in ipairs(chunkList) do
        dataChunk(chunk, index)
        maxCount = index
    end

    data:extend({{
        type = "flying-text",
        name = "DATA_RAW_COUNT",
        time_to_live = 0,
        speed = 1,
        order = maxCount
    }})
end

doStuff()
