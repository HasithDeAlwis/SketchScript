import { Link } from "react-router";
import { PencilRuler, Twitter, Github } from "lucide-react";
import { footerConstants } from "./footer.constants";
import * as React from "react";

export function Footer() {
  const iconMap: Record<string, React.ReactNode> = {
    PencilRuler: <PencilRuler className="w-5 h-5 text-primary" />,
    twitter: <Twitter className="w-5 h-5" />,
    github: <Github className="w-5 h-5" />,
  };

  return (
    <footer className="py-12 border-t border-muted">
      <div className="container">
        <div className="flex flex-col items-center justify-between md:flex-row">
          <div className="flex items-center mb-6 md:mb-0">
            <div className="relative flex items-center justify-center w-8 h-8 mr-2 transform rounded-lg bg-secondary rotate-3">
              {iconMap[footerConstants.logo.icon]}
            </div>
            <span className="text-xl font-bold text-primary">{footerConstants.logo.name}</span>
          </div>

          <div className="flex flex-wrap justify-center mb-6 gap-x-8 gap-y-4 md:mb-0">
            {footerConstants.navigation.map((nav) => (
              <Link
                key={nav.label}
                to={nav.href}
                className="text-sm transition-colors text-foreground/70 hover:text-foreground"
              >
                {nav.label}
              </Link>
            ))}
          </div>

          <div className="flex space-x-4">
            {footerConstants.socialLinks.map((social) => (
              <Link
                key={social.label}
                to={social.href}
                className="transition-colors text-foreground/70 hover:text-foreground"
              >
                {iconMap[social.icon]}
              </Link>
            ))}
          </div>
        </div>

        <div className="pt-8 mt-8 text-sm text-center border-t border-muted text-foreground/60">
          <p>© {footerConstants.copyright.year} {footerConstants.copyright.company}. All rights reserved.</p>
          <div className="flex justify-center mt-2 space-x-4">
            {footerConstants.copyright.links.map((link, idx) => (
              <React.Fragment key={link.label}>
                <Link to={link.href} className="transition-colors hover:text-foreground">
                  {link.label}
                </Link>
                {idx < footerConstants.copyright.links.length - 1 && <span>•</span>}
              </React.Fragment>
            ))}
          </div>
          <div className="mt-2">{footerConstants.copyright.tagline}</div>
        </div>
      </div>
    </footer>
  );
}
