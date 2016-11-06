-- pathToFile = "/some/path/to/show.dat"
-- episodeId = 5

function savePlaybackStateToFile(content)
    file = io.open(pathToFile, "w")
    io.output(file)
    io.write(episodeId .. "\n" .. content)
    io.close(file)
end

function persist_playback_state()
    --  for some reaseon, mpv may start ~15 later than the requested position
    -- lets work around it with a simple substraction
    playbackTime = math.max(0, mp.get_property("playback-time") - 15)
    savePlaybackStateToFile(playbackTime)
end

mp.add_periodic_timer(20, persist_playback_state)
