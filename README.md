# Automated Slide Deck Generation (Phase 4 – Shiny App)

## Overview

This README documents the Phase 4 implementation of the LIFE Matrix project, where the automated slide pipeline from Phase 3 was extended with a **Shiny web application**. The app allows users to interactively build slide instructions, preview charts, and export a complete PowerPoint deck without editing R code.

## Features

* **Instruction Builder**: Sidebar inputs for metrics, categories, subsets, and chart options.
* **Live Preview**: Real-time ggplot chart preview inside the app.
* **Deck Export**: Append slides to a working deck and download as `.pptx`.
* **Registry System**: Central mapping of slide functions to UI/server modules for easy extension.
* **Custom Styling**: Unified SCSS/CSS for consistent layout and design.