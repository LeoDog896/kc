use anyhow::Result;
use expr::*;
use pest::Parser;

mod expr;

fn main() -> Result<()> {
    let mut rl = rustyline::Editor::<()>::new()?;
    loop {
        let line = rl.readline(">> ")?;
        let pairs = CalculatorParser::parse(Rule::equation, &line);

        match pairs {
            Ok(mut pairs) => {
                let parsable_pair = pairs.next().unwrap().into_inner();
                let expr = parse_expr(parsable_pair);

                rl.add_history_entry(line.as_str());
                println!("{}", serialize_expression(&expr));
            }
            Err(error) => eprintln!("{}", error),
        }
    }
}
