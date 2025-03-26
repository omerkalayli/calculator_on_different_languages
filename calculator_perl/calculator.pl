use strict;
use warnings;

print "Perl Calculator\n";

while (1) {
    print "> ";
    my $input = <STDIN>;
    chomp($input);

    $input =~ s/\s//g;

    my @expression;
    my $processed = "";
    my $left_par_count = 0;
    my $right_par_count = 0;

    for my $c (split //, $input) {
        if ($c =~ /\d/) {
            $processed .= $c;
        } else {
            if ($processed ne "") {
                push @expression, $processed;
                $processed = "";
            }
            if ($c eq "(") {
                $left_par_count++;
            } elsif ($c eq ")") {
                $right_par_count++;
            }
            push @expression, $c;
        }
    }

    if ($right_par_count != $left_par_count) {
        print "Error: Parentheses do not match!\n";
        next;
    }

    if ($processed ne "") {
        push @expression, $processed;
    }

    my $idx = 0;
    my $result = evaluate(\@expression, \$idx);

    if ($result == -999) {
        print "Error: Division by zero!\n";
    } else {
        print "Result: $result\n";
    }
}

sub evaluate {
    my ($expression, $idx_ref) = @_;
    my @inner_expression;

    while ($$idx_ref < @$expression) {
        if ($$expression[$$idx_ref] eq "(") {
            $$idx_ref++;
            my $result = evaluate($expression, $idx_ref);
            if ($result == -999) {
                return $result;
            }
            push @inner_expression, $result;
        } elsif ($$expression[$$idx_ref] eq ")") {
            $$idx_ref++;
            return calculate(\@inner_expression, 0);
        } else {
            push @inner_expression, $$expression[$$idx_ref];
            $$idx_ref++;
        }
    }

    return calculate(\@inner_expression, 0);
}

sub calculate {
    my ($expression, $idx) = @_;

    while ($idx < @$expression) {
        if ($$expression[$idx] eq "*" || $$expression[$idx] eq "/") {
            my $left = $$expression[$idx - 1];
            my $right = $$expression[$idx + 1];

            my $result = 0;
            if ($$expression[$idx] eq "*") {
                $result = $left * $right;
            } elsif ($$expression[$idx] eq "/") {
                if ($right == 0) {
                    return -999; # Division by zero
                } else {
                    $result = $left / $right;
                }
            }

            splice @$expression, $idx - 1, 3, $result;
            $idx = 0;
        } else {
            $idx++;
        }
    }

    # Addition and subtraction
    $idx = 0;
    while ($idx < @$expression) {
        if ($$expression[$idx] eq "+" || $$expression[$idx] eq "-") {
            my $left = $$expression[$idx - 1];
            my $right = $$expression[$idx + 1];

            my $result = 0;
            if ($$expression[$idx] eq "+") {
                $result = $left + $right;
            } elsif ($$expression[$idx] eq "-") {
                $result = $left - $right;
            }

            splice @$expression, $idx - 1, 3, $result;
            $idx = 0;
        } else {
            $idx++;
        }
    }

    return $$expression[0];
}