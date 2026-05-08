# Package Philosophy and Design Principles

## Overview

`riskworkflowr` is designed as a reproducible spatial risk workflow
package.

It is not intended to replace specialist R packages for geometry
processing, epidemiology, spatial statistics, mapping, or modelling.

Instead, the package aims to help analysts connect common spatial risk
analysis steps into a consistent and auditable workflow.

## Core workflow

The package is built around a simple workflow:

`point events -> spatial assignment -> aggregation/counts -> risk metrics -> mapping`
