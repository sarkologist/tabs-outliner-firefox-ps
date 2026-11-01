// The curated outline the AMO listing screenshots show: well-known, uncontroversial
// sites, no personal data. One live window (bold rows) plus saved history (grey
// rows) imported as a snapshot, so every row kind the listing talks about appears.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import type { Seed } from "../e2e/support/fake-browser";

// Favicons as Firefox reports them for a live tab: `data:` bytes. The PNGs are
// 16/32px icons fetched once from Google's favicon service and committed, so a
// run makes no network request and renders the same every time.
const icon = (site: string): string => {
  const file = site === "openstreetmap.org" ? `${site}.jpg` : `${site}.png`;
  const mime = file.endsWith(".jpg") ? "image/jpeg" : "image/png";
  const bytes = readFileSync(join("screenshots", "favicons", file)).toString("base64");
  return `data:${mime};base64,${bytes}`;
};

export const liveSeed: Seed = {
  windows: [
    {
      id: 1,
      focused: true,
      tabs: [
        { id: 11, url: "https://doc.rust-lang.org/book/", title: "The Rust Programming Language", favIconUrl: icon("doc.rust-lang.org"), active: true },
        { id: 12, url: "https://doc.rust-lang.org/std/iter/trait.Iterator.html", title: "Iterator in std::iter - Rust", favIconUrl: icon("doc.rust-lang.org") },
        { id: 13, url: "https://github.com/rust-lang/rustlings", title: "GitHub - rust-lang/rustlings: 🦀 Small exercises to get you used to reading and writing Rust code!", favIconUrl: icon("github.com") },
        { id: 14, url: "https://stackoverflow.com/questions/tagged/rust", title: "Newest 'rust' Questions - Stack Overflow", favIconUrl: icon("stackoverflow.com") },
        { id: 15, url: "https://developer.mozilla.org/en-US/docs/WebAssembly", title: "WebAssembly | MDN", favIconUrl: icon("developer.mozilla.org") },
      ],
    },
  ],
};

type Saved = { title: string; url?: string; site?: string; collapsed?: boolean; children?: Saved[] };

const tab = (title: string, url: string, site: string, children: Saved[] = []): Saved => ({ title, url, site, children });
const group = (title: string, children: Saved[], collapsed = false): Saved => ({ title, children, collapsed });

const wiki = (title: string, children: Saved[] = []) =>
  tab(`${title} - Wikipedia`, `https://en.wikipedia.org/wiki/${encodeURIComponent(title.replace(/ /g, "_"))}`, "en.wikipedia.org", children);

const saved: Saved[] = [
  group("Trip to Kyoto", [
    tab("Kyoto – Travel guide at Wikivoyage", "https://en.wikivoyage.org/wiki/Kyoto", "wikivoyage.org", [
      wiki("Fushimi Inari-taisha"),
      wiki("Kinkaku-ji"),
      wiki("Arashiyama"),
      wiki("Philosopher's Walk"),
    ]),
    tab("OpenStreetMap", "https://www.openstreetmap.org/#map=13/35.0116/135.7681", "openstreetmap.org"),
    tab("Duolingo - The world's best way to learn a language", "https://www.duolingo.com/", "duolingo.com"),
  ]),
  group("Astronomy", [
    // APOD has no favicon: its row shows the kind glyph, as the sidebar does
    { title: "Astronomy Picture of the Day", url: "https://apod.nasa.gov/apod/astropix.html" },
    wiki("James Webb Space Telescope", [
      tab("ESA - Webb", "https://www.esa.int/Science_Exploration/Space_Science/Webb", "esa.int"),
      wiki("Hubble Space Telescope"),
    ]),
    tab("Cosmology and astronomy | Khan Academy", "https://www.khanacademy.org/science/cosmology-and-astronomy", "khanacademy.org"),
    tab("Powers of Ten (1977) - YouTube", "https://www.youtube.com/watch?v=0fKBhvDjuy0", "youtube.com"),
  ]),
  group("Weekend baking", [
    wiki("Sourdough"),
    tab("Bread recipes | BBC Good Food", "https://www.bbcgoodfood.com/recipes/collection/bread-recipes", "bbcgoodfood.com"),
    tab("Serious Eats", "https://www.seriouseats.com/", "seriouseats.com"),
  ]),
  group("Reading list", [
    tab("Pride and Prejudice by Jane Austen | Project Gutenberg", "https://www.gutenberg.org/ebooks/1342", "gutenberg.org"),
    tab("Moby Dick; Or, The Whale by Herman Melville | Project Gutenberg", "https://www.gutenberg.org/ebooks/2701", "gutenberg.org"),
    tab("Internet Archive: Digital Library of Free & Borrowable Books", "https://archive.org/", "archive.org"),
  ]),
  group(
    "Programming",
    [
      tab("Python documentation", "https://docs.python.org/3/", "python.org", [
        tab("itertools — Functions creating iterators for efficient looping", "https://docs.python.org/3/library/itertools.html", "python.org"),
        tab("The Python Tutorial", "https://docs.python.org/3/tutorial/", "python.org"),
      ]),
      tab("Iterators - Rust By Example", "https://doc.rust-lang.org/rust-by-example/trait/iter.html", "doc.rust-lang.org"),
      tab("Welcome to Python.org", "https://www.python.org/", "python.org"),
    ],
    true
  ),
];

// The export snapshot format Import accepts (`{ nodes, roots }`), as closed history.
export function savedSnapshot(): string {
  const nodes: Record<string, unknown>[] = [];
  let seq = 0;
  const add = (s: Saved, parent: string | null): string => {
    const id = `demo${++seq}`;
    const node: Record<string, unknown> = {
      id,
      kind: s.url ? "tab" : "group",
      parent,
      children: [],
      title: s.title,
      customTitle: null,
      url: s.url ?? null,
      favIconUrl: s.site ? icon(s.site) : null,
      active: false,
      collapsed: s.collapsed ?? false,
      createdAt: 0,
      closedAt: null,
      tabId: null,
      windowId: null,
      sessionId: null,
    };
    nodes.push(node);
    node.children = (s.children ?? []).map((c) => add(c, id));
    return id;
  };
  const roots = saved.map((s) => add(s, null));
  return JSON.stringify({ nodes, roots });
}
