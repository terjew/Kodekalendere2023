using System.Text.RegularExpressions;

var bets = Regex.Matches(File.ReadAllText("bets.txt"), @"\[(\d), (\d\.\d+)\]")
    .Select(m => new { 
        GoalsBet = int.Parse(m.Groups[1].Value), 
        Odds = double.Parse(m.Groups[2].Value) 
    })
    .ToList();

var goalsScored = File.ReadAllText("goals.txt")
    .Split(",")
    .Select(s => int.Parse(s))
    .ToList();

var outcomes = bets.Zip(goalsScored);

double cash = 50_000;
double betFactor = 0.175f;

foreach(var outcome in outcomes)
{
    var bet = outcome.First;
    var result = outcome.Second;

    var amountBet = Math.Round(cash * betFactor);
    var winFactor = bet.GoalsBet > result ? 0.0f : bet.Odds;
    if (winFactor > 0) cash += Math.Round(amountBet * winFactor);
    else cash -= amountBet;
}

Console.WriteLine("Gjenværende stenger: " + cash);
Console.WriteLine("Antall stenger tapt: " + (50_000 - cash));
