with TEXT_IO; use TEXT_IO;
procedure CALCULATOR is
    package IO_INTEGER is new INTEGER_IO (NUM => Integer);
    A, B, C: Integer;
    OPERAND: CHARACTER;
begin
    while not END_OF_FILE(STANDARD_INPUT) loop
        IO_INTEGER.GET(A);
        GET(OPERAND); 
        IO_INTEGER.GET(B);
        
        case OPERAND is
            when '+' => C := A + B;
            when '-' => C := A - B;
            when '*' => C := A * B;
            when '/' => 
                if B /= 0 then
                    C := A / B;
                else
                    PUT_LINE("Error: Division by zero");
                    exit;
                end if;
            when others => 
                PUT_LINE("Error: Invalid operator");
                exit;
        end case;

        IO_INTEGER.PUT(C);
        NEW_LINE;
    end loop;
end CALCULATOR;