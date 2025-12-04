! Boolean Logic Circuit High-Performance Simulator
! Fortran 90 Implementation for Large-Scale Circuit Analysis

MODULE BooleanCircuitTypes
    IMPLICIT NONE
    
    ! Gate types enumeration
    INTEGER, PARAMETER :: GATE_AND = 1
    INTEGER, PARAMETER :: GATE_OR = 2
    INTEGER, PARAMETER :: GATE_NOT = 3
    INTEGER, PARAMETER :: GATE_NAND = 4
    INTEGER, PARAMETER :: GATE_NOR = 5
    INTEGER, PARAMETER :: GATE_XOR = 6
    INTEGER, PARAMETER :: GATE_XNOR = 7
    INTEGER, PARAMETER :: GATE_BUF = 8
    
    ! Maximum circuit parameters
    INTEGER, PARAMETER :: MAX_GATES = 10000
    INTEGER, PARAMETER :: MAX_INPUTS = 100
    INTEGER, PARAMETER :: MAX_OUTPUTS = 100
    INTEGER, PARAMETER :: MAX_VARIABLES = 26
    
    TYPE :: Gate
        INTEGER :: gate_type
        INTEGER :: num_inputs
        INTEGER, DIMENSION(10) :: input_wires
        INTEGER :: output_wire
        REAL :: delay
        LOGICAL :: computed
    END TYPE Gate
    
    TYPE :: Circuit
        INTEGER :: num_gates
        INTEGER :: num_inputs
        INTEGER :: num_outputs
        INTEGER :: num_wires
        TYPE(Gate), DIMENSION(MAX_GATES) :: gates
        INTEGER, DIMENSION(MAX_INPUTS) :: input_wires
        INTEGER, DIMENSION(MAX_OUTPUTS) :: output_wires
        LOGICAL, DIMENSION(MAX_GATES) :: wire_values
    END TYPE Circuit
    
END MODULE BooleanCircuitTypes

MODULE BooleanGateSimulation
    USE BooleanCircuitTypes
    IMPLICIT NONE
    
CONTAINS
    
    FUNCTION simulate_gate(gate_type, inputs, num_inputs) RESULT(output)
        INTEGER, INTENT(IN) :: gate_type, num_inputs
        LOGICAL, DIMENSION(:), INTENT(IN) :: inputs
        LOGICAL :: output
        INTEGER :: i
        
        SELECT CASE(gate_type)
            CASE(GATE_AND)
                output = .TRUE.
                DO i = 1, num_inputs
                    output = output .AND. inputs(i)
                END DO
                
            CASE(GATE_OR)
                output = .FALSE.
                DO i = 1, num_inputs
                    output = output .OR. inputs(i)
                END DO
                
            CASE(GATE_NOT)
                output = .NOT. inputs(1)
                
            CASE(GATE_NAND)
                output = .TRUE.
                DO i = 1, num_inputs
                    output = output .AND. inputs(i)
                END DO
                output = .NOT. output
                
            CASE(GATE_NOR)
                output = .FALSE.
                DO i = 1, num_inputs
                    output = output .OR. inputs(i)
                END DO
                output = .NOT. output
                
            CASE(GATE_XOR)
                output = inputs(1)
                DO i = 2, num_inputs
                    output = (output .AND. .NOT. inputs(i)) .OR. &
                             (.NOT. output .AND. inputs(i))
                END DO
                
            CASE(GATE_XNOR)
                output = inputs(1)
                DO i = 2, num_inputs
                    output = (output .AND. .NOT. inputs(i)) .OR. &
                             (.NOT. output .AND. inputs(i))
                END DO
                output = .NOT. output
                
            CASE(GATE_BUF)
                output = inputs(1)
                
            CASE DEFAULT
                output = .FALSE.
        END SELECT
        
    END FUNCTION simulate_gate
    
    SUBROUTINE evaluate_circuit(circuit, input_values, output_values)
        TYPE(Circuit), INTENT(INOUT) :: circuit
        LOGICAL, DIMENSION(:), INTENT(IN) :: input_values
        LOGICAL, DIMENSION(:), INTENT(OUT) :: output_values
        INTEGER :: i, j, wire_idx
        LOGICAL, DIMENSION(10) :: gate_inputs
        LOGICAL :: all_computed
        
        ! Initialize wire values with inputs
        DO i = 1, circuit%num_inputs
            wire_idx = circuit%input_wires(i)
            circuit%wire_values(wire_idx) = input_values(i)
        END DO
        
        ! Mark all gates as not computed
        DO i = 1, circuit%num_gates
            circuit%gates(i)%computed = .FALSE.
        END DO
        
        ! Iteratively compute gates
        DO WHILE (.TRUE.)
            all_computed = .TRUE.
            
            DO i = 1, circuit%num_gates
                IF (.NOT. circuit%gates(i)%computed) THEN
                    ! Check if all inputs are available
                    LOGICAL :: can_compute
                    can_compute = .TRUE.
                    
                    DO j = 1, circuit%gates(i)%num_inputs
                        wire_idx = circuit%gates(i)%input_wires(j)
                        ! Check if wire is an input or computed
                        IF (wire_idx > circuit%num_inputs) THEN
                            ! Find if this wire has been computed
                            INTEGER :: k
                            LOGICAL :: found
                            found = .FALSE.
                            DO k = 1, i - 1
                                IF (circuit%gates(k)%output_wire == wire_idx .AND. &
                                    circuit%gates(k)%computed) THEN
                                    found = .TRUE.
                                    EXIT
                                END IF
                            END DO
                            IF (.NOT. found) THEN
                                can_compute = .FALSE.
                                EXIT
                            END IF
                        END IF
                    END DO
                    
                    IF (can_compute) THEN
                        ! Gather inputs
                        DO j = 1, circuit%gates(i)%num_inputs
                            wire_idx = circuit%gates(i)%input_wires(j)
                            gate_inputs(j) = circuit%wire_values(wire_idx)
                        END DO
                        
                        ! Compute gate output
                        circuit%wire_values(circuit%gates(i)%output_wire) = &
                            simulate_gate(circuit%gates(i)%gate_type, &
                                        gate_inputs, circuit%gates(i)%num_inputs)
                        circuit%gates(i)%computed = .TRUE.
                    ELSE
                        all_computed = .FALSE.
                    END IF
                END IF
            END DO
            
            IF (all_computed) EXIT
        END DO
        
        ! Extract outputs
        DO i = 1, circuit%num_outputs
            wire_idx = circuit%output_wires(i)
            output_values(i) = circuit%wire_values(wire_idx)
        END DO
        
    END SUBROUTINE evaluate_circuit
    
    SUBROUTINE generate_truth_table(circuit, num_vars)
        TYPE(Circuit), INTENT(INOUT) :: circuit
        INTEGER, INTENT(IN) :: num_vars
        INTEGER :: i, j, num_combinations
        INTEGER, DIMENSION(MAX_VARIABLES) :: bit_pattern
        LOGICAL, DIMENSION(MAX_VARIABLES) :: input_values
        LOGICAL, DIMENSION(MAX_OUTPUTS) :: output_values
        
        num_combinations = 2 ** num_vars
        
        PRINT *, "Truth Table:"
        PRINT *, REPEAT("=", 60)
        
        ! Print header
        WRITE(*, '(A)', ADVANCE='NO') "Inputs: "
        DO i = 1, num_vars
            WRITE(*, '(A,I0,A)', ADVANCE='NO') "V", i, " "
        END DO
        WRITE(*, '(A)', ADVANCE='NO') " | Outputs: "
        DO i = 1, circuit%num_outputs
            WRITE(*, '(A,I0,A)', ADVANCE='NO') "O", i, " "
        END DO
        PRINT *
        PRINT *, REPEAT("-", 60)
        
        ! Generate all combinations
        DO i = 0, num_combinations - 1
            ! Convert i to binary
            DO j = 1, num_vars
                bit_pattern(j) = MOD(i / (2 ** (num_vars - j)), 2)
                input_values(j) = (bit_pattern(j) == 1)
            END DO
            
            ! Evaluate circuit
            CALL evaluate_circuit(circuit, input_values(1:num_vars), output_values)
            
            ! Print row
            WRITE(*, '(A)', ADVANCE='NO') "        "
            DO j = 1, num_vars
                IF (input_values(j)) THEN
                    WRITE(*, '(A)', ADVANCE='NO') "1  "
                ELSE
                    WRITE(*, '(A)', ADVANCE='NO') "0  "
                END IF
            END DO
            
            WRITE(*, '(A)', ADVANCE='NO') " |          "
            DO j = 1, circuit%num_outputs
                IF (output_values(j)) THEN
                    WRITE(*, '(A)', ADVANCE='NO') "1  "
                ELSE
                    WRITE(*, '(A)', ADVANCE='NO') "0  "
                END IF
            END DO
            PRINT *
        END DO
        
        PRINT *, REPEAT("=", 60)
        
    END SUBROUTINE generate_truth_table
    
    SUBROUTINE benchmark_circuit(circuit, num_vars, iterations)
        TYPE(Circuit), INTENT(INOUT) :: circuit
        INTEGER, INTENT(IN) :: num_vars, iterations
        LOGICAL, DIMENSION(MAX_VARIABLES) :: input_values
        LOGICAL, DIMENSION(MAX_OUTPUTS) :: output_values
        INTEGER :: i, j
        REAL :: start_time, end_time, elapsed_time
        
        ! Random test inputs
        DO i = 1, num_vars
            input_values(i) = MOD(i, 2) == 0
        END DO
        
        CALL CPU_TIME(start_time)
        
        DO i = 1, iterations
            CALL evaluate_circuit(circuit, input_values(1:num_vars), output_values)
        END DO
        
        CALL CPU_TIME(end_time)
        elapsed_time = end_time - start_time
        
        PRINT *, "Benchmark Results:"
        PRINT *, "  Iterations:", iterations
        PRINT *, "  Total time:", elapsed_time, "seconds"
        PRINT *, "  Time per evaluation:", elapsed_time / iterations * 1e6, "microseconds"
        PRINT *, "  Throughput:", iterations / elapsed_time, "evaluations/second"
        
    END SUBROUTINE benchmark_circuit
    
END MODULE BooleanGateSimulation

PROGRAM BooleanCircuitSimulator
    USE BooleanCircuitTypes
    USE BooleanGateSimulation
    IMPLICIT NONE
    
    TYPE(Circuit) :: test_circuit
    INTEGER :: i
    
    PRINT *, "Boolean Logic Circuit High-Performance Simulator"
    PRINT *, "Fortran 90 Implementation"
    PRINT *, REPEAT("=", 60)
    PRINT *
    
    ! Example 1: XOR gate using AND, OR, NOT gates
    PRINT *, "Example 1: XOR Gate (A XOR B = A'B + AB')"
    PRINT *, REPEAT("-", 60)
    
    test_circuit%num_gates = 5
    test_circuit%num_inputs = 2
    test_circuit%num_outputs = 1
    test_circuit%num_wires = 6
    
    ! Input wires
    test_circuit%input_wires(1) = 1
    test_circuit%input_wires(2) = 2
    
    ! Gate 1: NOT A
    test_circuit%gates(1)%gate_type = GATE_NOT
    test_circuit%gates(1)%num_inputs = 1
    test_circuit%gates(1)%input_wires(1) = 1
    test_circuit%gates(1)%output_wire = 3
    
    ! Gate 2: NOT B
    test_circuit%gates(2)%gate_type = GATE_NOT
    test_circuit%gates(2)%num_inputs = 1
    test_circuit%gates(2)%input_wires(1) = 2
    test_circuit%gates(2)%output_wire = 4
    
    ! Gate 3: A AND NOT B
    test_circuit%gates(3)%gate_type = GATE_AND
    test_circuit%gates(3)%num_inputs = 2
    test_circuit%gates(3)%input_wires(1) = 1
    test_circuit%gates(3)%input_wires(2) = 4
    test_circuit%gates(3)%output_wire = 5
    
    ! Gate 4: NOT A AND B
    test_circuit%gates(4)%gate_type = GATE_AND
    test_circuit%gates(4)%num_inputs = 2
    test_circuit%gates(4)%input_wires(1) = 3
    test_circuit%gates(4)%input_wires(2) = 2
    test_circuit%gates(4)%output_wire = 6
    
    ! Gate 5: OR the results
    test_circuit%gates(5)%gate_type = GATE_OR
    test_circuit%gates(5)%num_inputs = 2
    test_circuit%gates(5)%input_wires(1) = 5
    test_circuit%gates(5)%input_wires(2) = 6
    test_circuit%gates(5)%output_wire = 7
    
    ! Output wire
    test_circuit%output_wires(1) = 7
    
    CALL generate_truth_table(test_circuit, 2)
    PRINT *
    
    CALL benchmark_circuit(test_circuit, 2, 1000000)
    PRINT *
    PRINT *
    
    ! Example 2: Full Adder
    PRINT *, "Example 2: Full Adder (3 inputs, 2 outputs - Sum and Carry)"
    PRINT *, REPEAT("-", 60)
    
    test_circuit%num_gates = 8
    test_circuit%num_inputs = 3
    test_circuit%num_outputs = 2
    test_circuit%num_wires = 12
    
    ! Input wires: A, B, Cin
    test_circuit%input_wires(1) = 1
    test_circuit%input_wires(2) = 2
    test_circuit%input_wires(3) = 3
    
    ! XOR chain for sum
    test_circuit%gates(1)%gate_type = GATE_XOR
    test_circuit%gates(1)%num_inputs = 2
    test_circuit%gates(1)%input_wires(1) = 1
    test_circuit%gates(1)%input_wires(2) = 2
    test_circuit%gates(1)%output_wire = 4
    
    test_circuit%gates(2)%gate_type = GATE_XOR
    test_circuit%gates(2)%num_inputs = 2
    test_circuit%gates(2)%input_wires(1) = 4
    test_circuit%gates(2)%input_wires(2) = 3
    test_circuit%gates(2)%output_wire = 10  ! Sum output
    
    ! Carry logic
    test_circuit%gates(3)%gate_type = GATE_AND
    test_circuit%gates(3)%num_inputs = 2
    test_circuit%gates(3)%input_wires(1) = 1
    test_circuit%gates(3)%input_wires(2) = 2
    test_circuit%gates(3)%output_wire = 5
    
    test_circuit%gates(4)%gate_type = GATE_AND
    test_circuit%gates(4)%num_inputs = 2
    test_circuit%gates(4)%input_wires(1) = 2
    test_circuit%gates(4)%input_wires(2) = 3
    test_circuit%gates(4)%output_wire = 6
    
    test_circuit%gates(5)%gate_type = GATE_AND
    test_circuit%gates(5)%num_inputs = 2
    test_circuit%gates(5)%input_wires(1) = 1
    test_circuit%gates(5)%input_wires(2) = 3
    test_circuit%gates(5)%output_wire = 7
    
    test_circuit%gates(6)%gate_type = GATE_OR
    test_circuit%gates(6)%num_inputs = 2
    test_circuit%gates(6)%input_wires(1) = 5
    test_circuit%gates(6)%input_wires(2) = 6
    test_circuit%gates(6)%output_wire = 8
    
    test_circuit%gates(7)%gate_type = GATE_OR
    test_circuit%gates(7)%num_inputs = 2
    test_circuit%gates(7)%input_wires(1) = 8
    test_circuit%gates(7)%input_wires(2) = 7
    test_circuit%gates(7)%output_wire = 11  ! Carry output
    
    ! Output wires: Sum, Carry
    test_circuit%output_wires(1) = 10
    test_circuit%output_wires(2) = 11
    
    CALL generate_truth_table(test_circuit, 3)
    PRINT *
    
    CALL benchmark_circuit(test_circuit, 3, 1000000)
    
END PROGRAM BooleanCircuitSimulator