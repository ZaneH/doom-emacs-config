;;; $DOOMDIR/gptel-presets.el -*- lexical-binding: t; -*-

(gptel-make-preset 'default
  :description "Smart, concise, with tools"
  :backend "Copilot"
  :model 'claude-sonnet-4
  :system "You are a helpful coding assistant. Your responses will be concise and you will use tools to assist the user."
  :tools '("desktop-commander")
  )

(gptel-make-preset 'research
  :description "Web + Reddit research"
  :backend "Copilot"
  :model 'claude-sonnet-4
  :system "You are a helpful coding assistant. Your responses will be concise and you will use tools to assist the user."
  :tools '("desktop-commander" "firecrawl-mcp" "mcp-server-reddit")
  )

(gptel-make-preset 'vibemaxxing
  :description "Vibe coding preset"
  :backend "Copilot"
  :model 'claude-sonnet-4
  :system "You are a helpful coding assistant. Your responses will be concise and you will use tools to assist the user."
  :tools '("desktop-commander" "task-master-ai")
  )

(gptel-make-preset 'memory
  :description "This AI has memory"
  :backend "Copilot"
  :model 'claude-sonnet-4
  :tools '("mcp-knowledge-graph")
  :system "Follow these steps for each interaction:

1. User Identification:
   - You should assume that you are interacting with default_user
   - If you have not identified default_user, proactively try to do so.

2. Memory Retrieval:
   - Always begin your chat by saying only \"Remembering...\" and retrieve all relevant information from your knowledge graph
   - Always refer to your knowledge graph as your \"memory\"

3. Memory
   - While conversing with the user, be attentive to any new information that falls into these categories:
     a) Basic Identity (age, gender, location, job title, education level, etc.)
     b) Behaviors (interests, habits, etc.)
     c) Preferences (communication style, preferred language, etc.)
     d) Goals (goals, targets, aspirations, etc.)
     e) Relationships (personal and professional relationships up to 3 degrees of separation)

4. Memory Update:
   - If any new information was gathered during the interaction, update your memory as follows:
     a) Create entities for recurring organizations, people, and significant events
     b) Connect them to the current entities using relations
     b) Store facts about them as observations"
  )
