;;; $DOOMDIR/+gptel.el -*- lexical-binding: t; -*-

;; Enable GPTel for AI conversations
(after! gptel
  (require 'gptel-integrations)
  (require 'mcp-hub)

  ;; Default GPTel model and backend
  (setq gptel-model 'claude-sonnet-4
        gptel-backend (gptel-make-gh-copilot "Copilot"))

  ;; Expert mode
  (setq gptel-expert-commands t)
  ;; Autoscroll when AI is typing
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

  ;; Provide API keys for additional GPTel backends
  ;; If secrets.el is provided, it will define these functions
  (when (fboundp 'my/gemini-api-key)
    (gptel-make-gemini "Gemini"
      :key #'my/gemini-api-key
      :stream t))

  (when (fboundp 'my/anthropic-api-key)
    (gptel-make-anthropic "Claude"
      :key #'my/anthropic-api-key
      :stream t
      :models '(claude-sonnet-4
                claude-3.7-sonnet
                claude-3.5-sonnet)))

  (when (fboundp 'my/openrouter-api-key)
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key #'my/openrouter-api-key
      :models '(
                google/gemini-2.5-pro-preview
                google/gemini-2.5-flash-preview-05-20
                )))


  (let ((anthropic-key (when (fboundp 'my/anthropic-api-key)
                         (my/anthropic-api-key)))
        (openrouter-key (when (fboundp 'my/openrouter-api-key)
                          (my/openrouter-api-key)))
        (gemini-key (when (fboundp 'my/gemini-api-key)
                      (my/gemini-api-key))))

    ;; Enable MCP servers for AI interactions
    (setq mcp-hub-servers
          `(("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
            ("mcp-server-reddit" . (:command "uvx" :args ("mcp-server-reddit")))
            ("task-master-ai" . (:command "npx"
                                 :args ("-y" "--package=task-master-ai" "task-master-ai")
                                 :env (
                                       :ANTHROPIC_API_KEY ,anthropic-key
                                       :OPENROUTER_API_KEY ,openrouter-key
                                       :GOOGLE_API_KEY ,gemini-key)))
            ("desktop-commander" . (:command "npx"
                                    :args ("-y" "@wonderwhy-er/desktop-commander")))
            ("firecrawl-mcp" . (:command "npx"
                                :args ("-y" "firecrawl-mcp")
                                :env (
                                      :FIRECRAWL_API_URL "http://localhost:3002"
                                      :FIRECRAWL_RETRY_INITIAL_DELAY 8000
                                      :FIRECRAWL_RETRY_MAX_ATTEMPTS 10
                                      :FIRECRAWL_RETRY_BACKOFF_FACTOR 3
                                      )))
            ("mcp-knowledge-graph" . (:command "npx"
                                      :args ("-y" "mcp-knowledge-graph" "--memory-path" "/home/me/repos/personal/mcp-knowledge-graph/memory.jsonl")))
            )
          ))

  ;; GPTel Presets
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

  )
