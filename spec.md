---
title: CS-DC Network
---

This document specifies the structure of the CS-DC network.
The goal of this network is to allow researcher to self-organize and collaborate in a decentralized manner.

# Fundamental entities

The fundamental entities of the CS-DC network are *people* and *units*.
People are related to units through a *membership* relation, and units are related between them through an *hierarchical* relation.
That is:

- A person can be or not a member of an unit.
- An unit can be or not a sub-unit of another unit.

Therefore, units are organized as a *graph*, whose nodes contains sets of people.

## People

Each person in the system should correspond to an actual person.
This makes sure that every vote has the same weight.
Every person can belong to multiple units, and take part in their administration.

## Units

Units are structures that allow researchers to organize and collaborate.
Each unit has a set of members, and can have sub-units.
Every member of a sub-unit is also a member of the unit itself.
This guarantees a democratical system that is inclusive at all levels.

The organization of an unit can be described with a parallel with the three powers:

- Every member of an unit has the right to vote for its internal legislation.
  This legislation defines what is the mechanism for changes in the unit, such as new members and new sub-units.
  The set of members compose the *legislative power*.

- Some elected members have special positions, which allow them to perform actions defined in the legislation.
  These elected members compose the *executive power*.

- All rules that can be written in a legislation are mechanically applied and verified.
  The system itself is the *judicial power*.

The set of special positions is pre-defined by the CS-DC chart.

# Technical aspects

There are three possible types changes in an unit:

- Changes of executive positions.
- Changes of permissions of executive positions.
- Changes of members.

The mechanics of these are defined in the legislation of the unit.
