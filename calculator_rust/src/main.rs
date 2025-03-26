use std::io::{self, Write};

fn main() {
    println!("Rust Calculator");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let input = input.trim().replace(" ", "");

        let mut processed = String::new();
        let mut expression: Vec<String> = Vec::new();

        // To check left right paranthesis equality.
        let mut left_par_count = 0;
        let mut right_par_count = 0;

        // Turn input string into lexeme array.
        for c in input.chars() {
            if c.is_digit(10) {
                processed.push(c);
            } else {
                if !processed.is_empty() {
                    expression.push(processed);
                    processed = String::new(); 
                }
                if c == '(' {
                    left_par_count += 1;
                } else if c == ')' {
                    right_par_count += 1;
                }
                expression.push(c.to_string());
            }
        }

        if right_par_count != left_par_count {
            println!("Error: Paranthesis do not match!");
            continue;
        }

        if !processed.is_empty() {
            expression.push(processed);
        }
        
        let mut idx = 0;
        let result = evaluate(&mut expression, &mut idx);

        if result == -999 {
            println!("Error: Division by zero!");   
        } else {
            println!("Result: {}", result);
        }
    }
}

// Evaluate operations in parantheses recursively.
fn evaluate(expression: &mut Vec<String>, idx: &mut usize) -> i32 {
    let mut inner_expression: Vec<String> = Vec::new(); 
    while *idx < expression.len() {
        if expression[*idx] == "(" {
            *idx += 1;
            let result = evaluate(expression, idx);
            if result == -999 { // If divide by zero return this special number.
                return result;
            }
            inner_expression.push(result.to_string());
        } else if expression[*idx] == ")" {
            *idx += 1;
            return calculate(&mut inner_expression, &mut 0);
        } else {
            inner_expression.push(expression[*idx].clone());
            *idx += 1;
        }
    }
    calculate(&mut inner_expression, &mut 0)
}

fn calculate(expression: &mut Vec<String>, idx: &mut usize) -> i32 {
    
    // Multiplication and division
    while *idx < expression.len() {
        if expression[*idx] == "*" || expression[*idx] == "/" {
            let left = expression[*idx - 1].parse::<i32>().unwrap();
            let right = expression[*idx + 1].parse::<i32>().unwrap();
            let result = match expression[*idx].as_str() {
                "*" => left * right,
                "/" => {
                    if right == 0 {
                        return -999; 
                    } else {
                        left / right
                    }
                }
                _ => 0,
            };
            
            expression.splice(*idx - 1..=*idx + 1, [result.to_string()]);
            *idx = 0;
        } else {
            *idx += 1;
        }
    }

    // Addition and substraction
    *idx = 0;
    while *idx < expression.len() {
        if expression[*idx] == "+" || expression[*idx] == "-" {
            let left = expression[*idx - 1].parse::<i32>().unwrap();
            let right = expression[*idx + 1].parse::<i32>().unwrap();
            let result = match expression[*idx].as_str() {
                "+" => left + right,
                "-" => left - right,
                _ => 0,
            };
            
            expression.splice(*idx - 1..=*idx + 1, [result.to_string()]);
            *idx = 0;
        } else {
            *idx += 1;
        }
    }
    expression[0].parse::<i32>().unwrap()
}
