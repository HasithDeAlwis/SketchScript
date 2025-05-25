import { useEffect, useState } from "react"
// import { useEffect } from "react"
import { ChevronDown, User, PencilRuler } from "lucide-react"
import { useLoaderData } from 'react-router';
import { DropdownMenu, DropdownMenuContent, DropdownMenuItem, DropdownMenuTrigger } from "@shared/ui/dropdown-menu"
import { useUpdateProjectMutation } from '../../../api/projects';
import { Project } from '../../../models/project';

// import { useSelector } from 'react-redux';
// import { RootState } from '../../../stores/index';

// interface EditorHeaderProps {
//     initialProjectName?: string
//     workspaces?: Workspace[]
// }


export function EditorHeader() {
    const workspaces = []
    const { project } = useLoaderData() as { project: Project }
    const [projectName, setProjectName] = useState(project.project_name || "Untitled Project");
    const [updateProject] = useUpdateProjectMutation();

    useEffect(() => {
        if (projectName !== project.project_name) {
            const timeout = setTimeout(() => {
                updateProject({
                    ...project,
                    project_name: projectName,
                });
            }, 500);

            return () => clearTimeout(timeout);
        }
    }, [projectName]);

    return (
        <header className="flex items-center justify-between h-12 px-4 border-b border-muted bg-background">
            <div className="flex items-center space-x-4">
                {/* Logo */}
                <div className="flex items-center">
                    <div className="relative flex items-center justify-center w-6 h-6 transform rounded-md bg-secondary rotate-3">
                        <PencilRuler className="w-3.5 h-3.5 text-primary" />
                    </div>
                </div>

                {/* Project name */}
                <div className="flex items-center">
                    <input
                        type="text"
                        value={projectName}
                        onChange={(e) => setProjectName(e.target.value)}
                        className="bg-transparent border-0 focus:ring-0 focus:outline-none font-medium text-foreground max-w-[200px]"
                    />
                </div>

                {/* Workspace dropdown */}
                <div className="opacity-50 pointer-events-none">

                    <DropdownMenu>
                        <DropdownMenuTrigger className="flex items-center px-2 py-1 text-sm transition-colors rounded text-foreground/70 hover:text-foreground hover:bg-muted/50">
                            Change workspace
                            <ChevronDown className="w-4 h-4 ml-1" />
                        </DropdownMenuTrigger>
                        <DropdownMenuContent align="start">
                            {workspaces.length > 0 ? (
                                workspaces.map((workspace) => (
                                    <DropdownMenuItem
                                        key={workspace.id}
                                        // onClick={() => onWorkspaceChange?.(workspace.id)}
                                        className="cursor-pointer"
                                    >
                                        {workspace.name}
                                    </DropdownMenuItem>
                                ))
                            ) : (
                                <DropdownMenuItem disabled>No workspaces available</DropdownMenuItem>
                            )}
                        </DropdownMenuContent>
                    </DropdownMenu>
                </div>
            </div>

            {/* User icon */}
            <div className="flex items-center">
                <div className="flex items-center justify-center w-8 h-8 rounded-full bg-accent/20">
                    <User className="w-4 h-4 text-foreground/70" />
                </div>
            </div>
        </header>
    )
}
