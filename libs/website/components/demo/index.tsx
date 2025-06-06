import type React from "react"

import { useState, useRef } from "react"
import { Button } from "@shared/ui/button"
import { Card, CardContent } from "@shared/ui/card"
import { Play, Pause, Volume2 } from "lucide-react"
import { demoConstants } from "./demo.constants"

export function DemoSection() {
  const [isPlaying, setIsPlaying] = useState(false)
  const [currentTime, setCurrentTime] = useState(0)
  const [duration, setDuration] = useState(0)
  const videoRef = useRef<HTMLVideoElement>(null)

  const togglePlay = () => {
    if (videoRef.current) {
      if (isPlaying) {
        videoRef.current.pause()
      } else {
        videoRef.current.play()
      }
      setIsPlaying(!isPlaying)
    }
  }

  const handleTimeUpdate = () => {
    if (videoRef.current) {
      setCurrentTime(videoRef.current.currentTime)
    }
  }

  const handleLoadedMetadata = () => {
    if (videoRef.current) {
      setDuration(videoRef.current.duration)
    }
  }

  const handleSeek = (e: React.MouseEvent<HTMLDivElement>) => {
    if (videoRef.current) {
      const rect = e.currentTarget.getBoundingClientRect()
      const clickX = e.clientX - rect.left
      const newTime = (clickX / rect.width) * duration
      videoRef.current.currentTime = newTime
      setCurrentTime(newTime)
    }
  }

  const formatTime = (time: number) => {
    const minutes = Math.floor(time / 60)
    const seconds = Math.floor(time % 60)
    return `${minutes}:${seconds.toString().padStart(2, "0")}`
  }

  return (
    <section className="py-20 bg-muted/20">
      <div className="container">
        <div className="mb-16 text-center">
          <h2 className="mb-4 text-3xl font-bold md:text-4xl">{demoConstants.title}</h2>
          <p className="max-w-2xl mx-auto text-lg text-foreground/80">{demoConstants.subtitle}</p>
        </div>

        <div className="max-w-4xl mx-auto">
          <Card className="overflow-hidden shadow-lg border-muted/50">
            <CardContent className="p-0">
              <div className="relative overflow-hidden bg-black rounded-lg">
                <video
                  ref={videoRef}
                  className="object-cover w-full aspect-video"
                  poster={demoConstants.video.poster}
                  onTimeUpdate={handleTimeUpdate}
                  onLoadedMetadata={handleLoadedMetadata}
                  onEnded={() => setIsPlaying(false)}
                >
                  <source src={demoConstants.video.src} type="video/mp4" />
                  Your browser does not support the video tag.
                </video>

                {/* Video overlay controls */}
                <div className="absolute inset-0 flex items-center justify-center transition-opacity duration-300 opacity-0 bg-black/20 hover:opacity-100">
                  <Button
                    onClick={togglePlay}
                    size="lg"
                    className="w-16 h-16 text-black rounded-full shadow-lg bg-white/90 hover:bg-white"
                  >
                    {isPlaying ? <Pause className="w-6 h-6" /> : <Play className="w-6 h-6 ml-1" />}
                  </Button>
                </div>

                {/* Video controls bar */}
                <div className="absolute bottom-0 left-0 right-0 p-4 bg-gradient-to-t from-black/80 to-transparent">
                  <div className="flex items-center space-x-4">
                    <Button onClick={togglePlay} size="sm" variant="ghost" className="p-2 text-white hover:bg-white/20">
                      {isPlaying ? <Pause className="w-4 h-4" /> : <Play className="w-4 h-4" />}
                    </Button>

                    <div className="flex items-center flex-1 space-x-2">
                      <span className="text-xs text-white">{formatTime(currentTime)}</span>
                      <div className="flex-1 h-1 rounded-full cursor-pointer bg-white/30" onClick={handleSeek}>
                        <div
                          className="h-full transition-all duration-150 rounded-full bg-secondary"
                          style={{ width: `${duration ? (currentTime / duration) * 100 : 0}%` }}
                        />
                      </div>
                      <span className="text-xs text-white">{formatTime(duration)}</span>
                    </div>

                    <Button size="sm" variant="ghost" className="p-2 text-white hover:bg-white/20">
                      <Volume2 className="w-4 h-4" />
                    </Button>
                  </div>
                </div>
              </div>
            </CardContent>
          </Card>

          {/* Features list */}
          <div className="grid grid-cols-2 gap-4 mt-8 md:grid-cols-4">
            {demoConstants.features.map((feature, index) => (
              <div key={index} className="flex items-center space-x-2 text-sm text-foreground/80">
                <div className="w-2 h-2 rounded-full bg-secondary"></div>
                <span>{feature}</span>
              </div>
            ))}
          </div>
        </div>
      </div>
    </section>
  )
}
