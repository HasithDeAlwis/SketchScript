import { useState, useEffect } from "react"
import { Link } from "react-router";
import { PencilRuler, Menu, X } from "lucide-react"
import { Button } from "@shared/ui/button"
import { ThemeToggle } from "@shared/utils/theme-toggle"
import { cn } from "@shared/utils"
import { navbarConstants } from "./navbar.constants"

export function Navbar() {
  const [isScrolled, setIsScrolled] = useState(false)
  const [isMobileMenuOpen, setIsMobileMenuOpen] = useState(false)

  useEffect(() => {
    const handleScroll = () => {
      setIsScrolled(window.scrollY > 10)
    }
    window.addEventListener("scroll", handleScroll)
    return () => window.removeEventListener("scroll", handleScroll)
  }, [])

  const iconMap: Record<string, React.ReactNode> = {
    PencilRuler: <PencilRuler className="w-5 h-5 text-background" />,
  }

  function getButtonVariant(variant: string | undefined) {
    switch (variant) {
      case "outline":
        return "outline";
      case "ghost":
        return "ghost";
      case "secondary":
        return "secondary";
      case "destructive":
        return "destructive";
      case "link":
        return "link";
      default:
        return undefined;
    }
  }

  return (
    <header
      className={cn(
        "fixed top-0 left-0 right-0 z-50 transition-all duration-300",
        isScrolled ? "bg-background/80 backdrop-blur-md py-2 shadow-sm" : "bg-transparent py-4",
      )}
    >
      <div className="container flex items-center justify-between">
        <Link to="/" className="flex items-center space-x-2">
          <div className="relative flex items-center justify-center w-8 h-8 transform rounded-lg bg-secondary rotate-3">
            {iconMap[navbarConstants.logo.icon]}
          </div>
          <span className="text-xl font-bold text-primary">{navbarConstants.logo.name}</span>
        </Link>

        {/* Desktop Navigation */}
        <nav className="items-center hidden space-x-8 md:flex">
          {navbarConstants.navigation.map((nav) => (
            <Link
              key={nav.label}
              to={nav.href}
              className="text-sm transition-colors text-foreground/80 hover:text-foreground"
            >
              {nav.label}
            </Link>
          ))}
          <div className="flex items-center space-x-2">
            <ThemeToggle />
            {navbarConstants.actions.map((action) => (
              <Button
                asChild
                key={action.label}
                size="sm"
                variant={getButtonVariant(action.variant)}
                className={
                  action.variant === "secondary"
                    ? "rounded-full bg-secondary text-background hover:bg-secondary/90"
                    : "rounded-full"
                }
              >
                <Link
                  to={action.href}
                  target={action.target}
                >
                  {action.label}
                </Link>
              </Button>
            ))}
          </div>
        </nav>

        {/* Mobile Menu Button */}
        <div className="flex items-center space-x-2 md:hidden">
          <ThemeToggle />
          <Button
            variant="ghost"
            size="icon"
            onClick={() => setIsMobileMenuOpen(!isMobileMenuOpen)}
            aria-label="Toggle menu"
          >
            {isMobileMenuOpen ? <X className="w-5 h-5" /> : <Menu className="w-5 h-5" />}
          </Button>
        </div>
      </div>

      {/* Mobile Menu */}
      {isMobileMenuOpen && (
        <div className="absolute left-0 right-0 p-4 border-b shadow-lg md:hidden top-full bg-background border-muted">
          <nav className="flex flex-col space-y-4">
            {navbarConstants.navigation.map((nav) => (
              <Link
                key={nav.label}
                to={nav.href}
                className="transition-colors text-foreground/80 hover:text-foreground"
                onClick={() => setIsMobileMenuOpen(false)}
              >
                {nav.label}
              </Link>
            ))}
            <div className="flex flex-col pt-2 space-y-2 border-t border-muted">
              {navbarConstants.actions.map((action) => (
                <Button
                  asChild
                  key={action.label}
                  variant={getButtonVariant(action.variant)}
                  className={
                    "justify-start w-full " +
                    (action.variant === "secondary"
                      ? "bg-secondary text-background hover:bg-secondary/90"
                      : "")
                  }
                >
                  <Link
                    to={action.href}
                    target={action.target}
                    onClick={() => setIsMobileMenuOpen(false)}
                  >
                    {action.label}
                  </Link>
                </Button>
              ))}
            </div>
          </nav>
        </div>
      )}
    </header>
  )
}
