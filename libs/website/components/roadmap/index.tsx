import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@shared/ui/card"
import { Badge } from "@shared/ui/badge"
import { roadmapConstants } from "./roadmap.constants"

export function RoadmapSection() {
  return (
    <section id="roadmap" className="py-20 bg-muted/20">
      <div className="container">
        <div className="mb-16 text-center">
          <h2 className="mb-4 text-3xl font-bold md:text-4xl">{roadmapConstants.title}</h2>
          <p className="max-w-2xl mx-auto text-lg text-foreground/80">
            {roadmapConstants.subtitle}
          </p>
        </div>

        <div className="max-w-3xl mx-auto">
          {roadmapConstants.roadmapData.map((item, index) => (
            <div key={item.id} className="relative pb-10 pl-10 last:pb-0">
              {index < roadmapConstants.roadmapData.length - 1 && <div className="timeline-connector"></div>}
              <div className="timeline-dot"></div>

              <Card
                className={`border-l-4 ${item.status === "completed"
                  ? "border-l-green-400 dark:border-l-green-600"
                  : item.status === "in-progress"
                    ? "border-l-secondary"
                    : "border-l-muted"
                  }`}
              >
                <CardHeader className="pb-2">
                  <div className="flex items-start justify-between">
                    <CardTitle className="text-lg">{item.title}</CardTitle>
                    <Badge
                      variant={
                        item.status === "completed"
                          ? "default"
                          : item.status === "in-progress"
                            ? "secondary"
                            : "outline"
                      }
                    >
                      {item.status === "completed"
                        ? "Completed"
                        : item.status === "in-progress"
                          ? "In Progress"
                          : "Planned"}
                    </Badge>
                  </div>
                  <CardDescription>{item.date}</CardDescription>
                </CardHeader>
                <CardContent>
                  <p className="text-foreground/80">{item.description}</p>
                </CardContent>
              </Card>
            </div>
          ))}
        </div>
      </div>
    </section>
  )
}
