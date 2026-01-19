# ePtDirty - Erlang Record Dirty Flag Maintenance Tool

[简体中文](README.md) | [English](README_EN.md)

`ePtDirty` is an Erlang Parse Transform tool. It automatically scans code during compilation, intercepts **Record Update** expressions, and injects logic to maintain a `dirtyFlag` (dirty field bitmap).

This tool is primarily used in game development or high-frequency data interaction scenarios to facilitate **incremental storage** (writing back to the database) or **incremental synchronization** (sending to clients).

---

## 1. Quick Start

### 1.1 Add Compilation Option

Add the following compilation option to the module where you need to automatically maintain dirty flags.

**Option A: Add in Source File**

```erlang
-module(my_player).
-compile({parse_transform, ePtDirty}).
```

**Option B: Add Globally in Rebar3 Config (`rebar.config`)**

```erlang
{erl_opts, [
    debug_info,
    {parse_transform, ePtDirty}
]}.
```

### 1.2 Define Record

Your Record definition must include a field named `dirtyFlag`.

> **Note**: If this field is missing, ePtDirty will ignore the Record without any side effects.

```erlang
-record(player, {
   id,
   hp,
   mp,
   %% [Required] Must specify a default integer value (0 is recommended)
   dirtyFlag = 0
}).
```

### 1.3 Write Code

You only need to write standard Erlang update code, and the compiler will handle the rest:

```erlang
%% Source Code
update_hp(P0) ->
   P0#player{hp = 100}.

%% Compiled Equivalent Code (Pseudo-code)
update_hp(P0) ->
   P0#player{hp = 100, dirtyFlag = P0#player.dirtyFlag bor Mask}.
```

Where `Mask` is calculated by assigning a bit to each field based on the order of fields in the record definition. The `Mask` is obtained by performing a `bor` operation on the bits corresponding to the fields appearing in the update.

---

## 2. Critical Limitations

To ensure code safety and performance, this tool has the following explicit limitations. Please adhere to them when using it.

### ⚠️ 1. `dirtyFlag` Must Have an Integer Default Value

* **Rule**: The `dirtyFlag` in the Record definition must have a default value, and it must be an integer.
* **Bad Example**: `-record(bad, {id, dirtyFlag}).` (Default value is `undefined`)
* **Consequence**: Executing `undefined bor Mask` at runtime will throw a **`badarith`** error, causing the process to crash.

### ⚠️ 2. Wildcard `_` Dirty Flag Capture Not Supported

* **Rule**: When using `_ = Val` to batch reset fields, the dirty flags of the reset fields will **not** be triggered.
* **Example**: `P1 = P0#player{_ = 0, id = 5}.`
* **Result**: Only `id` will be marked as dirty; other fields reset to 0 will **not** be marked.
* **Reason**: The parse transform cannot infer at compile time which specific fields `_` covers.

### ⚠️ 3. `setelement` Dynamic Update Not Supported

* **Rule**: Using `setelement/3` to update Record fields will **not** trigger dirty flags.
* **Reason**: `setelement` is a runtime function call, whereas the Parse Transform only processes `#rec{}` syntax structures at compile time.

### ⚠️ 4. Only Handles "Updates", Not "Creations"

* **Rule**: Only `Var#rec{...}` (Update) will be injected; `#rec{...}` (Create) will **not** be injected.
* **Example**: `P = #player{hp = 100}.` -> `dirtyFlag` remains the default value 0.
* **Reason**: In Erlang, the AST structures for "Creation" and "Pattern Matching" are identical. Forced injection would break pattern matching logic (e.g., `f(#player{hp=1}) -> ...`).
* **Suggestion**: If marking is needed upon creation, please specify manually: `#player{hp=100, dirtyFlag=4}`.

### ⚠️ 5. Temporary Variable Name Conflict Risk (Extremely Low)

* **Scenario**: When the subject of an update is a complex expression (e.g., `(get_player())#player{hp=1}`), the tool generates a temporary variable to prevent the expression from executing twice.
* **Mechanism**: The generated variable name is `__PtDirtyT` + `Counter` (e.g., `__PtDirtyT9966`).
* **Risk**: Unless the user intentionally defines a variable with the same name, the probability of conflict is almost zero.

### ⚠️ 6. Compile-time Atom Consumption

* **Scenario**: The aforementioned temporary variable names are generated via `list_to_atom/1`.
* **Impact**:
    * **Normal Compilation**: No impact. After compilation ends, the compiler process terminates, and Atom memory is released.
    * **Dynamic Compilation**: If your system frequently calls the compiler module to dynamically generate and load a large amount of different code at **runtime**, there may be a risk of Atom accumulation. This impact is negligible for standard CI/CD builds or hot-update processes.

### ⚠️ 7. Failure Fallback Strategy

* **Mechanism**: If an uncaught exception (Crash) occurs during the transformation process, the tool will catch the error and **downgrade to return the original code**.
* **Consequence**: Compilation will pass, but **the dirty flag function will silently fail**.
* **Monitoring**: Please be sure to check the compilation logs for warnings starting with `Warning: ePtDirty ParseTransform Failed`.

---

## 3. Supported Scenarios

The following complex scenarios have been tested and are handled correctly:

### ✅ 1. Complex Expression Updates (Parentheses Recommended)

The plugin automatically generates temporary variables to ensure the left-value expression executes only once.

```erlang
(get_player(Id))#player{hp = 0}.
%% Equivalent to:
%% T = get_player(Id),
%% T#player{hp=0, dirtyFlag = ...}
```

### ✅ 2. Nested Structure Updates

Supports updates within `fun`, `case`, `receive`, `try...catch`, and List Comprehensions.

```erlang
[P#player{hp = Max} || P <- AllPlayers].
```

### ✅ 3. Macros

Supports Record updates contained within macro definitions. The Parse Transform runs after macro expansion.

```erlang
-define(HEAL(P), P#player{hp = 100}).
f(P) -> ?HEAL(P).
```

### ✅ 4. Manual Override

If the user code manually specifies `dirtyFlag`, the plugin will **respect the user's intent** and skip automatic injection.

```erlang
P#player{hp = 100, dirtyFlag = 0}. %% dirtyFlag result is 0
```

---

## 4. Field Bit Allocation Rules (Technical Details)

The bitmask calculation relies entirely on the **definition order of the Record fields**.

* **Start Bit**: Starts from `2` (i.e., `1 bsl 1`). Bit 1 is usually reserved for special purposes or to avoid confusion.
* **Algorithm**: For every field encountered (including `dirtyFlag` itself), shift once: `Bit = Bit bsl 1`.

**Example:**

```erlang
-record(player, {
   id,          %% Bit: 2
   name,        %% Bit: 4
   level,       %% Bit: 8
   dirtyFlag,   %% Bit: 16 (Itself usually not marked, but occupies a place)
   gold         %% Bit: 32
}).
```

> **🚨 Warning**:
> **Adjusting the order of Record fields will directly change the Bit mapping values!**
> If your system uses dirtyFlag for persistent storage or cross-version protocol synchronization, adjusting the field order may lead to data interpretation errors. Please be careful.

---

## 5. Best Practices

1. **Database Write-back Strategy**:
   Check `dirtyFlag` during persistence.
   ```erlang
   %% Pseudo-code: save only hp and gold
   NeedSave = (P#player.dirtyFlag band (4 bor 32)) > 0.
   ```

2. **Resetting Flags**:
   After data saving or synchronization is complete, be sure to reset the `dirtyFlag` in memory.
   ```erlang
   P_Clean = P_Dirty#player{dirtyFlag = 0}.
   ```

3. **Field Count Control**:
   Although Erlang Integers support arbitrary size, when used as a bitmask, it is recommended to keep the number of Record fields within a reasonable range (e.g., within 1000) to maintain efficient bitwise operation performance and code readability.
