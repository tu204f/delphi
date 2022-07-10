(*******************************************************************************
  Тупа по умолчание работы
*******************************************************************************)
unit UnitBotTrades;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Candel.SysUtils,
  BotTrade;



type
  ///<summary></summary>
  TTradeBots = TObjectList<TDefaultBot>;

  TApiBots = record
  private
    class function InitBot(const AProfit, AStop: Integer): TDefaultBot; static;
    class procedure InitTradeBots; static;
    class procedure FinalTradeBots; static;
    class function GetBots: TTradeBots; static;
  public
    class procedure StartWork(const ABeginProfit, AStepProfit, AEndProfit, ABeginStop, AStepStop, AEndStop: Integer); static;
  public
    class procedure ExecutionStart; static;
    class procedure ExecutionLastCandel(const ACandel: TCandel); static;
    class procedure ExecutionStop; static;
    class property Bots: TTradeBots read GetBots;
  end;

implementation

var
  localTradeBots: TTradeBots;

{ TApiBots }

class procedure TApiBots.InitTradeBots;
begin
  if not Assigned(localTradeBots) then
    localTradeBots := TTradeBots.Create;
end;

class procedure TApiBots.FinalTradeBots;
begin
  FreeAndNil(localTradeBots);
end;

class function TApiBots.GetBots: TTradeBots;
begin
  Result := localTradeBots;
end;

class function TApiBots.InitBot(const AProfit, AStop: Integer): TDefaultBot;
begin
  var xBot := TDefaultBot.Create;
  xBot.ID := localTradeBots.Add(xBot);
  xBot.Init( AProfit,AStop);
  Result := xBot;
end;

class procedure TApiBots.StartWork(const ABeginProfit, AStepProfit, AEndProfit, ABeginStop, AStepStop, AEndStop: Integer);
var
  xProfit, xStop: Integer;
begin
  xProfit := ABeginProfit;
  while xProfit <= AEndProfit do
  begin
    xStop   := ABeginStop;
    while xStop <= AEndStop do
    begin
      InitBot(xProfit,xStop);
      Inc(xStop,AStepStop);
    end;
    Inc(xProfit,AStepProfit);
  end;
end;

class procedure TApiBots.ExecutionLastCandel(const ACandel: TCandel);
begin
  for var xBot in localTradeBots do
    xBot.ExecutionLastCandel(ACandel);
end;

class procedure TApiBots.ExecutionStart;
begin
  for var xBot in localTradeBots do
    xBot.ExecutionStart;
end;

class procedure TApiBots.ExecutionStop;
begin
  {todo: Нужно будет за фиссировать для истории, состояние изменение объекта}
  for var xBot in localTradeBots do
    xBot.ExecutionStop;
end;

initialization
  TApiBots.InitTradeBots;

finalization
  TApiBots.FinalTradeBots;

end.
