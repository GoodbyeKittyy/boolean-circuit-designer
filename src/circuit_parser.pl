#!/usr/bin/perl
# Boolean Logic Circuit Parser and Optimizer
# Perl Implementation for Expression Parsing and Gate-Level Netlist Generation

use strict;
use warnings;
use Data::Dumper;

package BooleanParser;

sub new {
    my ($class, $expression) = @_;
    my $self = {
        expression => $expression // '',
        variables => [],
        tokens => [],
        ast => undef,
        netlist => []
    };
    bless $self, $class;
    return $self;
}

sub tokenize {
    my ($self) = @_;
    my $expr = $self->{expression};
    my @tokens;
    
    while ($expr =~ /\S/) {
        if ($expr =~ /^([A-Za-z])/) {
            push @tokens, {type => 'VAR', value => $1};
            $expr = substr($expr, 1);
        }
        elsif ($expr =~ /^(\*)/) {
            push @tokens, {type => 'AND', value => $1};
            $expr = substr($expr, 1);
        }
        elsif ($expr =~ /^(\+)/) {
            push @tokens, {type => 'OR', value => $1};
            $expr = substr($expr, 1);
        }
        elsif ($expr =~ /^(')/) {
            push @tokens, {type => 'NOT', value => $1};
            $expr = substr($expr, 1);
        }
        elsif ($expr =~ /^(⊕)/) {
            push @tokens, {type => 'XOR', value => $1};
            $expr = substr($expr, 1);
        }
        elsif ($expr =~ /^(\()/) {
            push @tokens, {type => 'LPAREN', value => $1};
            $expr = substr($expr, 1);
        }
        elsif ($expr =~ /^(\))/) {
            push @tokens, {type => 'RPAREN', value => $1};
            $expr = substr($expr, 1);
        }
        elsif ($expr =~ /^\s+/) {
            $expr =~ s/^\s+//;
        }
        else {
            die "Unexpected character: " . substr($expr, 0, 1) . "\n";
        }
    }
    
    $self->{tokens} = \@tokens;
    return \@tokens;
}

sub extract_variables {
    my ($self) = @_;
    my %vars;
    
    foreach my $token (@{$self->{tokens}}) {
        if ($token->{type} eq 'VAR') {
            $vars{$token->{value}} = 1;
        }
    }
    
    my @sorted_vars = sort keys %vars;
    $self->{variables} = \@sorted_vars;
    return \@sorted_vars;
}

sub parse_expression {
    my ($self) = @_;
    $self->tokenize();
    $self->extract_variables();
    
    my $pos = 0;
    
    my $parse_or = sub {
        my $parse_or_ref;
        my $parse_and_ref;
        my $parse_xor_ref;
        my $parse_not_ref;
        my $parse_primary_ref;
        
        $parse_primary_ref = sub {
            if ($pos >= @{$self->{tokens}}) {
                die "Unexpected end of expression\n";
            }
            
            my $token = $self->{tokens}[$pos];
            
            if ($token->{type} eq 'VAR') {
                $pos++;
                return {type => 'VAR', value => $token->{value}};
            }
            elsif ($token->{type} eq 'LPAREN') {
                $pos++;
                my $node = $parse_or_ref->();
                if ($pos >= @{$self->{tokens}} || $self->{tokens}[$pos]{type} ne 'RPAREN') {
                    die "Expected closing parenthesis\n";
                }
                $pos++;
                return $node;
            }
            else {
                die "Expected variable or '('\n";
            }
        };
        
        $parse_not_ref = sub {
            my $node = $parse_primary_ref->();
            
            while ($pos < @{$self->{tokens}} && $self->{tokens}[$pos]{type} eq 'NOT') {
                $pos++;
                $node = {type => 'NOT', operand => $node};
            }
            
            return $node;
        };
        
        $parse_and_ref = sub {
            my $left = $parse_not_ref->();
            
            while ($pos < @{$self->{tokens}} && $self->{tokens}[$pos]{type} eq 'AND') {
                $pos++;
                my $right = $parse_not_ref->();
                $left = {type => 'AND', left => $left, right => $right};
            }
            
            return $left;
        };
        
        $parse_xor_ref = sub {
            my $left = $parse_and_ref->();
            
            while ($pos < @{$self->{tokens}} && $self->{tokens}[$pos]{type} eq 'XOR') {
                $pos++;
                my $right = $parse_and_ref->();
                $left = {type => 'XOR', left => $left, right => $right};
            }
            
            return $left;
        };
        
        $parse_or_ref = sub {
            my $left = $parse_xor_ref->();
            
            while ($pos < @{$self->{tokens}} && $self->{tokens}[$pos]{type} eq 'OR') {
                $pos++;
                my $right = $parse_xor_ref->();
                $left = {type => 'OR', left => $left, right => $right};
            }
            
            return $left;
        };
        
        return $parse_or_ref->();
    };
    
    $self->{ast} = $parse_or->();
    return $self->{ast};
}

sub evaluate {
    my ($self, $node, $values) = @_;
    
    if ($node->{type} eq 'VAR') {
        return $values->{$node->{value}} // 0;
    }
    elsif ($node->{type} eq 'NOT') {
        return !$self->evaluate($node->{operand}, $values);
    }
    elsif ($node->{type} eq 'AND') {
        return $self->evaluate($node->{left}, $values) && $self->evaluate($node->{right}, $values);
    }
    elsif ($node->{type} eq 'OR') {
        return $self->evaluate($node->{left}, $values) || $self->evaluate($node->{right}, $values);
    }
    elsif ($node->{type} eq 'XOR') {
        my $l = $self->evaluate($node->{left}, $values);
        my $r = $self->evaluate($node->{right}, $values);
        return ($l && !$r) || (!$l && $r);
    }
    
    return 0;
}

sub generate_truth_table {
    my ($self) = @_;
    
    if (!$self->{ast}) {
        $self->parse_expression();
    }
    
    my @vars = @{$self->{variables}};
    my $n = scalar @vars;
    my @table;
    
    for (my $i = 0; $i < (2 ** $n); $i++) {
        my %values;
        for (my $j = 0; $j < $n; $j++) {
            $values{$vars[$j]} = ($i >> ($n - 1 - $j)) & 1;
        }
        
        my $output = $self->evaluate($self->{ast}, \%values) ? 1 : 0;
        push @table, {%values, OUTPUT => $output};
    }
    
    return \@table;
}

sub generate_netlist {
    my ($self) = @_;
    my @netlist;
    my $wire_counter = 0;
    
    my $generate_gates;
    $generate_gates = sub {
        my ($node) = @_;
        
        if ($node->{type} eq 'VAR') {
            return $node->{value};
        }
        elsif ($node->{type} eq 'NOT') {
            my $input = $generate_gates->($node->{operand});
            my $output = "w" . $wire_counter++;
            push @netlist, {gate => 'NOT', inputs => [$input], output => $output};
            return $output;
        }
        elsif ($node->{type} eq 'AND' || $node->{type} eq 'OR' || $node->{type} eq 'XOR') {
            my $left_wire = $generate_gates->($node->{left});
            my $right_wire = $generate_gates->($node->{right});
            my $output = "w" . $wire_counter++;
            push @netlist, {
                gate => $node->{type},
                inputs => [$left_wire, $right_wire],
                output => $output
            };
            return $output;
        }
    };
    
    if ($self->{ast}) {
        my $output = $generate_gates->($self->{ast});
        push @netlist, {gate => 'OUTPUT', inputs => [$output], output => 'OUT'};
    }
    
    $self->{netlist} = \@netlist;
    return \@netlist;
}

sub to_vhdl {
    my ($self, $entity_name) = @_;
    $entity_name //= 'boolean_circuit';
    
    my @vars = @{$self->{variables}};
    my $vhdl = "library IEEE;\n";
    $vhdl .= "use IEEE.STD_LOGIC_1164.ALL;\n\n";
    $vhdl .= "entity $entity_name is\n";
    $vhdl .= "    Port (\n";
    
    foreach my $var (@vars) {
        $vhdl .= "        $var : in STD_LOGIC;\n";
    }
    
    $vhdl .= "        OUTPUT : out STD_LOGIC\n";
    $vhdl .= "    );\n";
    $vhdl .= "end $entity_name;\n\n";
    $vhdl .= "architecture Behavioral of $entity_name is\n";
    
    my @netlist = @{$self->{netlist}};
    foreach my $gate (@netlist) {
        if ($gate->{output} =~ /^w\d+$/) {
            $vhdl .= "    signal " . $gate->{output} . " : STD_LOGIC;\n";
        }
    }
    
    $vhdl .= "begin\n";
    
    foreach my $gate (@netlist) {
        if ($gate->{gate} eq 'NOT') {
            $vhdl .= "    " . $gate->{output} . " <= not " . $gate->{inputs}[0] . ";\n";
        }
        elsif ($gate->{gate} eq 'AND') {
            $vhdl .= "    " . $gate->{output} . " <= " . $gate->{inputs}[0] . " and " . $gate->{inputs}[1] . ";\n";
        }
        elsif ($gate->{gate} eq 'OR') {
            $vhdl .= "    " . $gate->{output} . " <= " . $gate->{inputs}[0] . " or " . $gate->{inputs}[1] . ";\n";
        }
        elsif ($gate->{gate} eq 'XOR') {
            $vhdl .= "    " . $gate->{output} . " <= " . $gate->{inputs}[0] . " xor " . $gate->{inputs}[1] . ";\n";
        }
        elsif ($gate->{gate} eq 'OUTPUT') {
            $vhdl .= "    OUTPUT <= " . $gate->{inputs}[0] . ";\n";
        }
    }
    
    $vhdl .= "end Behavioral;\n";
    
    return $vhdl;
}

sub print_summary {
    my ($self) = @_;
    
    print "=== Boolean Circuit Parser - Perl Implementation ===\n\n";
    print "Expression: " . $self->{expression} . "\n";
    print "Variables: " . join(", ", @{$self->{variables}}) . "\n\n";
    
    print "Truth Table:\n";
    print "-" x 50 . "\n";
    
    my $table = $self->generate_truth_table();
    my @vars = @{$self->{variables}};
    
    print join("\t", @vars, "OUTPUT") . "\n";
    print "-" x 50 . "\n";
    
    foreach my $row (@$table) {
        my @values = map { $row->{$_} } @vars;
        push @values, $row->{OUTPUT};
        print join("\t", @values) . "\n";
    }
    
    print "\n" . "=" x 50 . "\n";
    print "Gate-level Netlist:\n";
    print "-" x 50 . "\n";
    
    foreach my $gate (@{$self->{netlist}}) {
        print $gate->{gate} . " [" . join(", ", @{$gate->{inputs}}) . "] -> " . $gate->{output} . "\n";
    }
    
    print "\n";
}

package main;

# Main execution
sub main {
    print "Boolean Logic Circuit Designer - Perl Implementation\n";
    print "=====================================================\n\n";
    
    my @test_expressions = (
        "A*B",
        "A+B",
        "A*B'+A'*B",
        "(A+B)*(C+D)",
        "A*B*C+A'*B'*C'"
    );
    
    foreach my $expr (@test_expressions) {
        print "\nProcessing: $expr\n";
        print "=" x 60 . "\n";
        
        my $parser = BooleanParser->new($expr);
        $parser->parse_expression();
        $parser->generate_netlist();
        $parser->print_summary();
        
        print "\nVHDL Code:\n";
        print "-" x 60 . "\n";
        print $parser->to_vhdl();
        print "\n" . "=" x 60 . "\n\n";
    }
}

main() unless caller();