package com.circuit.designer.controller;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpStatus;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Boolean Logic Circuit Designer & Simulator - Spring Boot REST API
 * Professional-grade backend service for digital circuit design and analysis
 */

@SpringBootApplication
@RestController
@RequestMapping("/api/circuit")
@CrossOrigin(origins = "*")
public class BooleanCircuitController {

    public static void main(String[] args) {
        SpringApplication.run(BooleanCircuitController.class, args);
    }

    // ==================== Data Models ====================

    static class BooleanExpression {
        private String expression;
        private List<Character> variables;
        
        public BooleanExpression() {}
        
        public BooleanExpression(String expression) {
            this.expression = expression;
            this.variables = extractVariables(expression);
        }
        
        private List<Character> extractVariables(String expr) {
            Set<Character> vars = new HashSet<>();
            for (char c : expr.toCharArray()) {
                if (Character.isLetter(c)) {
                    vars.add(c);
                }
            }
            return vars.stream().sorted().collect(Collectors.toList());
        }
        
        public String getExpression() { return expression; }
        public void setExpression(String expression) { 
            this.expression = expression;
            this.variables = extractVariables(expression);
        }
        public List<Character> getVariables() { return variables; }
    }

    static class TruthTableRow {
        private Map<Character, Integer> inputs;
        private int output;
        
        public TruthTableRow(Map<Character, Integer> inputs, int output) {
            this.inputs = inputs;
            this.output = output;
        }
        
        public Map<Character, Integer> getInputs() { return inputs; }
        public int getOutput() { return output; }
    }

    static class TruthTable {
        private List<Character> variables;
        private List<TruthTableRow> rows;
        
        public TruthTable(List<Character> variables, List<TruthTableRow> rows) {
            this.variables = variables;
            this.rows = rows;
        }
        
        public List<Character> getVariables() { return variables; }
        public List<TruthTableRow> getRows() { return rows; }
    }

    static class KarnaughMap {
        private int[][] grid;
        private List<Character> variables;
        private int size;
        
        public KarnaughMap(int[][] grid, List<Character> variables, int size) {
            this.grid = grid;
            this.variables = variables;
            this.size = size;
        }
        
        public int[][] getGrid() { return grid; }
        public List<Character> getVariables() { return variables; }
        public int getSize() { return size; }
    }

    static class CircuitAnalysis {
        private String originalExpression;
        private TruthTable truthTable;
        private KarnaughMap karnaughMap;
        private String minimizedExpression;
        private int complexity;
        private long evaluationTime;
        
        public CircuitAnalysis() {}
        
        // Getters and setters
        public String getOriginalExpression() { return originalExpression; }
        public void setOriginalExpression(String expr) { this.originalExpression = expr; }
        
        public TruthTable getTruthTable() { return truthTable; }
        public void setTruthTable(TruthTable table) { this.truthTable = table; }
        
        public KarnaughMap getKarnaughMap() { return karnaughMap; }
        public void setKarnaughMap(KarnaughMap kmap) { this.karnaughMap = kmap; }
        
        public String getMinimizedExpression() { return minimizedExpression; }
        public void setMinimizedExpression(String expr) { this.minimizedExpression = expr; }
        
        public int getComplexity() { return complexity; }
        public void setComplexity(int complexity) { this.complexity = complexity; }
        
        public long getEvaluationTime() { return evaluationTime; }
        public void setEvaluationTime(long time) { this.evaluationTime = time; }
    }

    static class Gate {
        private String type;
        private List<String> inputs;
        private String output;
        
        public Gate(String type, List<String> inputs, String output) {
            this.type = type;
            this.inputs = inputs;
            this.output = output;
        }
        
        public String getType() { return type; }
        public List<String> getInputs() { return inputs; }
        public String getOutput() { return output; }
    }

    static class Netlist {
        private List<Gate> gates;
        private List<String> inputs;
        private List<String> outputs;
        
        public Netlist() {
            this.gates = new ArrayList<>();
            this.inputs = new ArrayList<>();
            this.outputs = new ArrayList<>();
        }
        
        public List<Gate> getGates() { return gates; }
        public List<String> getInputs() { return inputs; }
        public List<String> getOutputs() { return outputs; }
    }

    // ==================== Core Logic ====================

    private int evaluateExpression(String expr, Map<Character, Integer> values) {
        try {
            String processed = expr;
            
            // Replace variables with values
            for (Map.Entry<Character, Integer> entry : values.entrySet()) {
                processed = processed.replace(String.valueOf(entry.getKey()), 
                                             entry.getValue().toString());
            }
            
            // Replace operators
            processed = processed.replace("*", "&&")
                                .replace("+", "||")
                                .replace("'", "!");
            
            // Handle XOR
            while (processed.contains("⊕")) {
                int idx = processed.indexOf("⊕");
                char left = processed.charAt(idx - 1);
                char right = processed.charAt(idx + 1);
                int xorResult = (left != right) ? 1 : 0;
                processed = processed.substring(0, idx - 1) + xorResult + 
                           processed.substring(idx + 2);
            }
            
            // Simple evaluation
            return evaluateBoolean(processed);
        } catch (Exception e) {
            return 0;
        }
    }

    private int evaluateBoolean(String expr) {
        expr = expr.trim();
        
        // Handle parentheses
        while (expr.contains("(")) {
            int lastOpen = expr.lastIndexOf('(');
            int nextClose = expr.indexOf(')', lastOpen);
            String sub = expr.substring(lastOpen + 1, nextClose);
            int subResult = evaluateBoolean(sub);
            expr = expr.substring(0, lastOpen) + subResult + expr.substring(nextClose + 1);
        }
        
        // Handle NOT
        while (expr.contains("!")) {
            int idx = expr.indexOf('!');
            char next = expr.charAt(idx + 1);
            int notResult = (next == '1') ? 0 : 1;
            expr = expr.substring(0, idx) + notResult + 
                   (idx + 2 < expr.length() ? expr.substring(idx + 2) : "");
        }
        
        // Handle AND
        while (expr.contains("&&")) {
            int idx = expr.indexOf("&&");
            char left = expr.charAt(idx - 1);
            char right = expr.charAt(idx + 2);
            int andResult = (left == '1' && right == '1') ? 1 : 0;
            expr = expr.substring(0, idx - 1) + andResult + 
                   (idx + 3 < expr.length() ? expr.substring(idx + 3) : "");
        }
        
        // Handle OR
        while (expr.contains("||")) {
            int idx = expr.indexOf("||");
            char left = expr.charAt(idx - 1);
            char right = expr.charAt(idx + 2);
            int orResult = (left == '1' || right == '1') ? 1 : 0;
            expr = expr.substring(0, idx - 1) + orResult + 
                   (idx + 3 < expr.length() ? expr.substring(idx + 3) : "");
        }
        
        return expr.charAt(0) == '1' ? 1 : 0;
    }

    private TruthTable generateTruthTable(BooleanExpression boolExpr) {
        List<Character> vars = boolExpr.getVariables();
        List<TruthTableRow> rows = new ArrayList<>();
        int numVars = vars.size();
        int numRows = (int) Math.pow(2, numVars);
        
        for (int i = 0; i < numRows; i++) {
            Map<Character, Integer> values = new HashMap<>();
            
            for (int j = 0; j < numVars; j++) {
                int bitValue = (i >> (numVars - 1 - j)) & 1;
                values.put(vars.get(j), bitValue);
            }
            
            int output = evaluateExpression(boolExpr.getExpression(), values);
            rows.add(new TruthTableRow(values, output));
        }
        
        return new TruthTable(vars, rows);
    }

    private KarnaughMap generateKarnaughMap(TruthTable truthTable) {
        List<Character> vars = truthTable.getVariables();
        int numVars = vars.size();
        
        if (numVars < 2 || numVars > 4) {
            return null;
        }
        
        int[][] grid;
        
        if (numVars == 2) {
            grid = new int[2][2];
            for (int i = 0; i < 4; i++) {
                int row = i / 2;
                int col = i % 2;
                grid[row][col] = truthTable.getRows().get(i).getOutput();
            }
        } else if (numVars == 3) {
            grid = new int[2][4];
            int[] grayOrder = {0, 1, 3, 2};
            for (int i = 0; i < 2; i++) {
                for (int j = 0; j < 4; j++) {
                    int idx = i * 4 + grayOrder[j];
                    grid[i][j] = truthTable.getRows().get(idx).getOutput();
                }
            }
        } else {
            grid = new int[4][4];
            int[] grayOrder = {0, 1, 3, 2, 4, 5, 7, 6, 12, 13, 15, 14, 8, 9, 11, 10};
            for (int i = 0; i < 4; i++) {
                for (int j = 0; j < 4; j++) {
                    grid[i][j] = truthTable.getRows().get(grayOrder[i * 4 + j]).getOutput();
                }
            }
        }
        
        return new KarnaughMap(grid, vars, numVars);
    }

    private int calculateComplexity(String expression) {
        int complexity = 0;
        for (char c : expression.toCharArray()) {
            if (c == '*' || c == '+' || c == '\'' || c == '⊕') {
                complexity++;
            }
        }
        return complexity;
    }

    // ==================== REST Endpoints ====================

    @PostMapping("/analyze")
    public ResponseEntity<CircuitAnalysis> analyzeCircuit(@RequestBody BooleanExpression expression) {
        try {
            long startTime = System.nanoTime();
            
            CircuitAnalysis analysis = new CircuitAnalysis();
            analysis.setOriginalExpression(expression.getExpression());
            
            TruthTable truthTable = generateTruthTable(expression);
            analysis.setTruthTable(truthTable);
            
            KarnaughMap kmap = generateKarnaughMap(truthTable);
            analysis.setKarnaughMap(kmap);
            
            analysis.setComplexity(calculateComplexity(expression.getExpression()));
            
            long endTime = System.nanoTime();
            analysis.setEvaluationTime((endTime - startTime) / 1000);
            
            return ResponseEntity.ok(analysis);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    @PostMapping("/evaluate")
    public ResponseEntity<Map<String, Object>> evaluateExpression(
            @RequestBody Map<String, Object> request) {
        try {
            String expression = (String) request.get("expression");
            @SuppressWarnings("unchecked")
            Map<String, Integer> values = (Map<String, Integer>) request.get("values");
            
            Map<Character, Integer> charValues = new HashMap<>();
            values.forEach((k, v) -> charValues.put(k.charAt(0), v));
            
            int result = evaluateExpression(expression, charValues);
            
            Map<String, Object> response = new HashMap<>();
            response.put("result", result);
            response.put("expression", expression);
            response.put("inputs", values);
            
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
        }
    }

    @GetMapping("/gates")
    public ResponseEntity<List<String>> getSupportedGates() {
        List<String> gates = Arrays.asList("AND", "OR", "NOT", "NAND", "NOR", "XOR", "XNOR");
        return ResponseEntity.ok(gates);
    }

    @PostMapping("/netlist")
    public ResponseEntity<Netlist> generateNetlist(@RequestBody BooleanExpression expression) {
        try {
            Netlist netlist = new Netlist();
            
            // Add inputs
            expression.getVariables().forEach(v -> netlist.getInputs().add(String.valueOf(v)));
            
            // Simple netlist generation
            Gate outputGate = new Gate("CIRCUIT", 
                expression.getVariables().stream()
                    .map(String::valueOf)
                    .collect(Collectors.toList()),
                "OUTPUT");
            netlist.getGates().add(outputGate);
            netlist.getOutputs().add("OUTPUT");
            
            return ResponseEntity.ok(netlist);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    @GetMapping("/health")
    public ResponseEntity<Map<String, String>> healthCheck() {
        Map<String, String> response = new HashMap<>();
        response.put("status", "UP");
        response.put("service", "Boolean Circuit Designer API");
        response.put("version", "1.0.0");
        return ResponseEntity.ok(response);
    }
}