import React, { useState, useRef, useEffect } from 'react';
import { Play, Trash2, Download, Upload, Zap } from 'lucide-react';

const BooleanCircuitSimulator = () => {
  const [expression, setExpression] = useState('');
  const [result, setResult] = useState(null);
  const [truthTable, setTruthTable] = useState([]);
  const [kmap, setKmap] = useState(null);
  const [error, setError] = useState('');
  const [simulationRunning, setSimulationRunning] = useState(false);

  const inputRef = useRef(null);

  const insertChar = (char) => {
    setExpression(prev => prev + char);
    setError('');
  };

  const clearExpression = () => {
    setExpression('');
    setResult(null);
    setTruthTable([]);
    setKmap(null);
    setError('');
  };

  const backspace = () => {
    setExpression(prev => prev.slice(0, -1));
  };

  const evaluateExpression = (expr, variables) => {
    try {
      let processedExpr = expr
        .replace(/\*/g, '&&')
        .replace(/\+/g, '||')
        .replace(/⊕/g, '^')
        .replace(/'/g, '!');
      
      const func = new Function(...Object.keys(variables), `return ${processedExpr} ? 1 : 0;`);
      return func(...Object.values(variables));
    } catch (e) {
      return null;
    }
  };

  const generateTruthTable = () => {
    if (!expression.trim()) {
      setError('Please enter a boolean expression');
      return;
    }

    setSimulationRunning(true);
    setError('');

    try {
      const variables = [...new Set(expression.match(/[A-Za-z]/g) || [])].sort();
      
      if (variables.length === 0) {
        setError('No variables found in expression');
        setSimulationRunning(false);
        return;
      }

      if (variables.length > 4) {
        setError('Maximum 4 variables supported');
        setSimulationRunning(false);
        return;
      }

      const rows = Math.pow(2, variables.length);
      const table = [];

      for (let i = 0; i < rows; i++) {
        const values = {};
        for (let j = 0; j < variables.length; j++) {
          values[variables[j]] = (i >> (variables.length - 1 - j)) & 1;
        }
        const output = evaluateExpression(expression, values);
        table.push({ ...values, output });
      }

      setTruthTable(table);
      
      if (variables.length <= 4) {
        generateKMap(table, variables);
      }

      setTimeout(() => setSimulationRunning(false), 500);
    } catch (e) {
      setError('Invalid expression: ' + e.message);
      setSimulationRunning(false);
    }
  };

  const generateKMap = (table, variables) => {
    if (variables.length === 2) {
      const km = [
        [table[0].output, table[1].output],
        [table[2].output, table[3].output]
      ];
      setKmap({ grid: km, vars: variables, size: 2 });
    } else if (variables.length === 3) {
      const km = [
        [table[0].output, table[1].output, table[3].output, table[2].output],
        [table[4].output, table[5].output, table[7].output, table[6].output]
      ];
      setKmap({ grid: km, vars: variables, size: 3 });
    } else if (variables.length === 4) {
      const order = [0, 1, 3, 2, 4, 5, 7, 6, 12, 13, 15, 14, 8, 9, 11, 10];
      const km = [
        [table[order[0]].output, table[order[1]].output, table[order[2]].output, table[order[3]].output],
        [table[order[4]].output, table[order[5]].output, table[order[6]].output, table[order[7]].output],
        [table[order[8]].output, table[order[9]].output, table[order[10]].output, table[order[11]].output],
        [table[order[12]].output, table[order[13]].output, table[order[14]].output, table[order[15]].output]
      ];
      setKmap({ grid: km, vars: variables, size: 4 });
    }
  };

  const evaluateCurrent = () => {
    if (!expression.trim()) {
      setError('Please enter a boolean expression');
      return;
    }

    const variables = [...new Set(expression.match(/[A-Za-z]/g) || [])].sort();
    const values = {};
    variables.forEach(v => values[v] = 1);
    
    const res = evaluateExpression(expression, values);
    setResult(res);
  };

  const exportCircuit = () => {
    const data = {
      expression,
      truthTable,
      kmap,
      timestamp: new Date().toISOString()
    };
    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'boolean_circuit.json';
    a.click();
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-gray-800 to-gray-900 p-8 flex items-center justify-center">
      <div className="w-full max-w-6xl">
        <div className="bg-gray-600 rounded-3xl shadow-2xl p-8 border-8 border-gray-700">
          <div className="text-center mb-6">
            <h1 className="text-3xl font-bold text-gray-200 mb-2">Boolean Logic Circuit Designer</h1>
            <p className="text-gray-400 text-sm">Professional Digital Logic Simulator</p>
          </div>

          <div className="bg-gray-800 rounded-xl p-6 mb-6 shadow-inner">
            <div className="bg-gray-900 rounded-lg p-4 mb-4 font-mono text-2xl text-green-400 min-h-16 flex items-center justify-end border-2 border-gray-700">
              {expression || '0'}
            </div>
            
            {result !== null && (
              <div className="bg-blue-900 rounded-lg p-3 mb-4 text-center">
                <span className="text-blue-200 font-semibold">Result: </span>
                <span className="text-blue-100 font-mono text-xl">{result}</span>
              </div>
            )}

            {error && (
              <div className="bg-red-900 rounded-lg p-3 mb-4 text-red-200 text-sm">
                {error}
              </div>
            )}

            <div className="grid grid-cols-6 gap-2 mb-4">
              {['A', 'B', 'C', 'D', 'E', 'F'].map(letter => (
                <button
                  key={letter}
                  onClick={() => insertChar(letter)}
                  className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg transition-all duration-200 hover:shadow-lg"
                >
                  {letter}
                </button>
              ))}
            </div>

            <div className="grid grid-cols-4 gap-2 mb-4">
              {['G', 'H', 'I', 'J', 'K', 'L', 'M', 'N'].map(letter => (
                <button
                  key={letter}
                  onClick={() => insertChar(letter)}
                  className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg transition-all duration-200 hover:shadow-lg"
                >
                  {letter}
                </button>
              ))}
            </div>

            <div className="grid grid-cols-5 gap-2 mb-4">
              <button onClick={() => insertChar('*')} className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg">AND (*)</button>
              <button onClick={() => insertChar('+')} className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg">OR (+)</button>
              <button onClick={() => insertChar("'")} className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg">NOT (')</button>
              <button onClick={() => insertChar('⊕')} className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg">XOR (⊕)</button>
              <button onClick={() => insertChar('(')} className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg">(</button>
            </div>

            <div className="grid grid-cols-4 gap-2 mb-4">
              <button onClick={() => insertChar(')')} className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg">)</button>
              <button onClick={() => insertChar('0')} className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg">0</button>
              <button onClick={() => insertChar('1')} className="bg-gray-700 hover:bg-gray-600 text-gray-200 font-bold py-3 rounded-lg">1</button>
              <button onClick={backspace} className="bg-orange-600 hover:bg-orange-500 text-white font-bold py-3 rounded-lg">←</button>
            </div>

            <div className="grid grid-cols-2 gap-2">
              <button
                onClick={clearExpression}
                className="bg-orange-600 hover:bg-orange-500 text-white font-bold py-4 rounded-lg flex items-center justify-center gap-2 transition-all duration-200"
              >
                <Trash2 size={20} /> Clear
              </button>
              <button
                onClick={generateTruthTable}
                className={`${simulationRunning ? 'bg-green-700' : 'bg-blue-600 hover:bg-blue-500'} text-white font-bold py-4 rounded-lg flex items-center justify-center gap-2 transition-all duration-200`}
                disabled={simulationRunning}
              >
                <Zap size={20} /> {simulationRunning ? 'Simulating...' : 'Simulate'}
              </button>
            </div>

            <div className="grid grid-cols-2 gap-2 mt-2">
              <button
                onClick={evaluateCurrent}
                className="bg-cyan-600 hover:bg-cyan-500 text-white font-bold py-3 rounded-lg flex items-center justify-center gap-2"
              >
                <Play size={18} /> Evaluate
              </button>
              <button
                onClick={exportCircuit}
                className="bg-purple-600 hover:bg-purple-500 text-white font-bold py-3 rounded-lg flex items-center justify-center gap-2"
              >
                <Download size={18} /> Export
              </button>
            </div>
          </div>

          {truthTable.length > 0 && (
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
              <div className="bg-gray-800 rounded-xl p-6 shadow-lg">
                <h2 className="text-xl font-bold text-gray-200 mb-4">Truth Table</h2>
                <div className="overflow-auto max-h-96">
                  <table className="w-full text-sm">
                    <thead>
                      <tr className="bg-gray-700">
                        {Object.keys(truthTable[0]).map((key, idx) => (
                          <th key={idx} className="p-2 text-gray-200 font-mono border border-gray-600">
                            {key === 'output' ? 'Out' : key}
                          </th>
                        ))}
                      </tr>
                    </thead>
                    <tbody>
                      {truthTable.map((row, idx) => (
                        <tr key={idx} className="hover:bg-gray-700">
                          {Object.values(row).map((val, i) => (
                            <td key={i} className={`p-2 text-center font-mono border border-gray-600 ${val === 1 ? 'text-green-400' : 'text-red-400'}`}>
                              {val}
                            </td>
                          ))}
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </div>

              {kmap && (
                <div className="bg-gray-800 rounded-xl p-6 shadow-lg">
                  <h2 className="text-xl font-bold text-gray-200 mb-4">Karnaugh Map</h2>
                  <div className="flex justify-center">
                    <div className="inline-block">
                      {kmap.size === 2 && (
                        <div className="grid grid-cols-3 gap-1">
                          <div></div>
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[1]}'</div>
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[1]}</div>
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[0]}'</div>
                          {kmap.grid[0].map((val, i) => (
                            <div key={i} className={`w-16 h-16 flex items-center justify-center font-mono text-xl border-2 ${val === 1 ? 'bg-green-900 text-green-200 border-green-700' : 'bg-gray-900 text-gray-400 border-gray-700'}`}>
                              {val}
                            </div>
                          ))}
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[0]}</div>
                          {kmap.grid[1].map((val, i) => (
                            <div key={i} className={`w-16 h-16 flex items-center justify-center font-mono text-xl border-2 ${val === 1 ? 'bg-green-900 text-green-200 border-green-700' : 'bg-gray-900 text-gray-400 border-gray-700'}`}>
                              {val}
                            </div>
                          ))}
                        </div>
                      )}
                      {kmap.size === 3 && (
                        <div className="grid grid-cols-5 gap-1">
                          <div></div>
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[1]}'{kmap.vars[2]}'</div>
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[1]}'{kmap.vars[2]}</div>
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[1]}{kmap.vars[2]}</div>
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[1]}{kmap.vars[2]}'</div>
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[0]}'</div>
                          {kmap.grid[0].map((val, i) => (
                            <div key={i} className={`w-14 h-14 flex items-center justify-center font-mono text-lg border-2 ${val === 1 ? 'bg-green-900 text-green-200 border-green-700' : 'bg-gray-900 text-gray-400 border-gray-700'}`}>
                              {val}
                            </div>
                          ))}
                          <div className="text-center text-gray-400 text-xs p-1">{kmap.vars[0]}</div>
                          {kmap.grid[1].map((val, i) => (
                            <div key={i} className={`w-14 h-14 flex items-center justify-center font-mono text-lg border-2 ${val === 1 ? 'bg-green-900 text-green-200 border-green-700' : 'bg-gray-900 text-gray-400 border-gray-700'}`}>
                              {val}
                            </div>
                          ))}
                        </div>
                      )}
                      {kmap.size === 4 && (
                        <div className="grid grid-cols-5 gap-1">
                          <div></div>
                          <div className="text-center text-gray-400 text-xs p-1">00</div>
                          <div className="text-center text-gray-400 text-xs p-1">01</div>
                          <div className="text-center text-gray-400 text-xs p-1">11</div>
                          <div className="text-center text-gray-400 text-xs p-1">10</div>
                          {kmap.grid.map((row, i) => (
                            <React.Fragment key={i}>
                              <div className="text-center text-gray-400 text-xs p-1">{['00', '01', '11', '10'][i]}</div>
                              {row.map((val, j) => (
                                <div key={j} className={`w-12 h-12 flex items-center justify-center font-mono border-2 ${val === 1 ? 'bg-green-900 text-green-200 border-green-700' : 'bg-gray-900 text-gray-400 border-gray-700'}`}>
                                  {val}
                                </div>
                              ))}
                            </React.Fragment>
                          ))}
                        </div>
                      )}
                    </div>
                  </div>
                </div>
              )}
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default BooleanCircuitSimulator;