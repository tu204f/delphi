unit Lb.WorkTraderThread;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.ReadPrice,
  Lb.SysUtils;

type
  ///<summary>Íà îñíîâàíèå ÷åãî ïğèíèìàşòñÿ ğåøåíèÿ</summary>
  TParams = class(TObject)
  public type
    TParam = TPair<String,Double>;
    TParamList = TList<TParam>;
  private
    FParams: TParamList;
    function GetValue(AName: String): Double;
    procedure SetValue(AName: String; const Value: Double);
  protected
    function IndexOf(const AName: String): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Value[AName: String]: Double read GetValue write SetValue;
  end;

  TOnEvent         = procedure(Sander: TObject) of object;
  TOnEventProgress = procedure(Sander: TObject; AProgress: Integer) of object;

  ///<summary>Ïîòîê äëÿ òåñòèğîâàíèå ïàğàìåòğîâ<summary>
  TWorkTraderThread = class(TThread)
  private
    FID: Integer;
    FMinusProfit: Double;
    FPeriodRSI: Integer;
    FFileName: String;
    FCandelsSource: TCandelsSource;
  private
    FTrader: TWorkTrader;
    FOnEventStart: TOnEvent;
    FOnEventStop: TOnEvent;
    FOnEventProgress: TOnEventProgress;
  private
    FSide: TTypeSide;
    FOpenRSI: Double;
    FOpenActiveRSI: Double;
    FCloseRSI: Double;
    FCloseActiveRSI: Double;
    FProfit: Double;
    procedure SetID(const Value: Integer);
  protected
    FProgresSource: Integer;
    procedure DoStartThread;
    procedure DoProgress;
    procedure DoStopThread;
  protected
    procedure Execute; override;
    procedure RandomParam;
    property CandelsSource: TCandelsSource read FCandelsSource;
  public
    constructor Create;
    destructor Destroy; override;
    property Trader: TWorkTrader read FTrader;
    property ID: Integer read FID write SetID;
    property PeriodRSI: Integer read FPeriodRSI write FPeriodRSI;
    property FileName: String read FFileName write FFileName;
    property MinusProfit: Double read FMinusProfit write FMinusProfit;
  public
    property OnEventStart: TOnEvent write FOnEventStart;
    property OnEventStop: TOnEvent write FOnEventStop;
    property OnEventProgress: TOnEventProgress write FOnEventProgress;
  public
    property Side: TTypeSide read FSide write FSide;
    property OpenRSI: Double read FOpenRSI write FOpenRSI;
    property OpenActiveRSI: Double read FOpenActiveRSI write FOpenActiveRSI;
    property CloseRSI: Double read FCloseRSI write FCloseRSI;
    property CloseActiveRSI: Double read FCloseActiveRSI write FCloseActiveRSI;
    property Profit: Double read FProfit write FProfit;
  end;

implementation

procedure SaveWorkTrader(const ATrader: TWorkTrader);

  procedure _TradeGrid(ASource: TStrings; APosition: TPositionTrade);
  var
    xS: String;
    i, iCount: Integer;
    xTrade: TTrade;
  begin
    iCount := APosition.Trades.Count;
    if iCount > 0 then
    begin

      for i := 0 to iCount - 1 do
      begin
        xTrade := APosition.Trades[i];
        xS := ';;' + DateToStr(xTrade.Date) + ';';
        xS := xS + TimeToStr(xTrade.Time) + ';';
        xS := xS + xTrade.Price.ToString + ';';
        xS := xS + xTrade.Qty.ToString + ';';
        xS := xS + GetTypeSideToStr(xTrade.Side) + ';';
        ASource.Add(xS);
      end;

      xTrade := APosition.CloseTrade;
      xS := ';;' + DateToStr(xTrade.Date) + ';';
      xS := xS + TimeToStr(xTrade.Time) + ';';
      xS := xS + xTrade.Price.ToString + ';';
      xS := xS + xTrade.Qty.ToString + ';';
      xS := xS + GetTypeSideToStr(xTrade.Side) + ';';
      ASource.Add(xS);
    end;
  end;

  procedure _PositionTrader(ASource: TStrings; ATrader: TWorkTrader);
  var
    xS: String;
    i, iCount: Integer;
    xPosition: TPositionTrade;
  begin
    iCount := ATrader.PositionTrades.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xPosition := ATrader.PositionTrades[i];
        xS := xPosition.Price.ToString + ';';
        xS := xS + xPosition.Qty.ToString + ';';
        xS := xS + GetTypeSideToStr(xPosition.Side) + ';';
        if xPosition.TypePosition = tpClose then
        begin
          xS := xS + xPosition.CloseTrade.Price.ToString + ';';
          xS := xS + xPosition.CloseTrade.Qty.ToString + ';';
          xS := xS + xPosition.Profit.ToString + ';';
        end;

        ASource.Add(xS);
        _TradeGrid(ASource, xPosition);
      end;
    end;
  end;

var
  xStr: TStrings;
  xPath: String;
begin
  xStr := TStringList.Create;
  try
    xPath := ExtractFilePath(ParamStr(0)) + 'result\';
    _PositionTrader(xStr,ATrader);
    xStr.SaveToFile(xPath + Format('trade_' + ATrader.ID.ToString + '.csv',[ATrader.ID]));
  finally
    FreeAndNil(xStr);
  end;
end;

{ TParams }

constructor TParams.Create;
begin
  FParams := TParamList.Create;
end;

destructor TParams.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TParams.IndexOf(const AName: String): Integer;
var
  xParam: TParam;
  i, iCount: Integer;
begin
  Result := -1;
  iCount := FParams.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xParam := FParams[i];
      if SameText(AName,xParam.Key) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TParams.GetValue(AName: String): Double;
var
  xIndex: Integer;
begin
  Result := 0;
  xIndex := IndexOf(AName);
  if xIndex >= 0 then
    Result := FParams[xIndex].Value;
end;

procedure TParams.SetValue(AName: String; const Value: Double);
var
  xParam: TParam;
  xIndex: Integer;
begin
  xIndex := IndexOf(AName);
  if xIndex >= 0 then
  begin
    xParam := FParams[xIndex];
    xParam.Value := Value;
    FParams[xIndex] := xParam;
  end
  else
  begin
    xParam.Key := AName;
    xParam.Value := Value;
    FParams.Add(xParam);
  end;
end;

{ TWorkTraderThread }

constructor TWorkTraderThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FPeriodRSI := 14;
  FCandelsSource := TCandelsSource.Create;
  FTrader := TWorkTrader.Create;
  FMinusProfit := -1000;
end;

destructor TWorkTraderThread.Destroy;
begin
  FreeAndNil(FTrader);
  FreeAndNil(FCandelsSource);
  inherited;
end;

procedure TWorkTraderThread.DoProgress;
begin
  if Assigned(FOnEventProgress) then
    FOnEventProgress(Self,FProgresSource);
end;

procedure TWorkTraderThread.DoStartThread;
begin
  if Assigned(FOnEventStart) then
    FOnEventStart(Self);
end;

procedure TWorkTraderThread.DoStopThread;
begin
  if Assigned(FOnEventStop) then
    FOnEventStop(Self);
end;

procedure TWorkTraderThread.Execute;
var
  i, iCount: Integer;
var
  xValRSI: Double;
  xCandel: TCandel;
  xCandels: TCandelList;
begin
  Synchronize(DoStartThread);

  FProfit := 0;
  CandelsSource.LoadFromFile(FFileName);
  CandelsSource.Delete(0);
  RandomParam;
  iCount := CandelsSource.Count;
  if iCount > 0 then
  begin
    FTrader.PositionTrades.Clear;
    for i := 0 to iCount - FPeriodRSI - 1 do
    begin
      xCandels := TCandelList.Create;
      try
        SetCandels(i, FPeriodRSI, CandelsSource, xCandels);
        xCandel := xCandels[FPeriodRSI - 1];
        xValRSI := GetRSI(xCandels);
        FTrader.SetUpDateCandel(xCandel,xValRSI);

        FProgresSource := Round((100 * i)/iCount);
        if Frac(i/100) = 0 then
          Synchronize(DoProgress);

        if FTrader.Profit < FMinusProfit then
          Break;
      finally
        FreeAndNil(xCandels);
      end;
    end;
    SaveWorkTrader(FTrader);
    FProfit := FTrader.Profit;
  end;

  Synchronize(DoStopThread);
end;

procedure TWorkTraderThread.RandomParam;
var
  xÑriterion: TÑriterion;
begin
  case FSide of
    tsBuy: begin
      FOpenRSI        := Random(100);
      FOpenActiveRSI  := FOpenRSI + Random(100);
      FCloseRSI       := Random(100);
      FCloseActiveRSI := FCloseRSI - Random(100);

      if FOpenActiveRSI > 100 then
        FOpenActiveRSI := 100;

      if FCloseActiveRSI < 0 then
        FCloseActiveRSI := 0;
    end;
    tsSell: begin
      FOpenRSI        := Random(100);
      FOpenActiveRSI  := FOpenRSI - Random(100);
      FCloseRSI       := Random(100);
      FCloseActiveRSI := FCloseRSI + Random(100);

      if FOpenActiveRSI < 0 then
        FOpenActiveRSI := 0;

      if FCloseActiveRSI > 100 then
        FCloseActiveRSI := 100;
    end;
  end;

  FTrader.Side := FSide;
  FTrader.CreateÑriterion(1);
  // ---------------------------------
  xÑriterion := FTrader.Ñriterions[0];
  xÑriterion.RSI := FOpenRSI;
  xÑriterion.ReActiveRSI := FOpenActiveRSI;
  xÑriterion.Qty := 1;
  xÑriterion.IsActive := True;
  // ---------------------------------
  xÑriterion := FTrader.Ñriterions[1];
  xÑriterion.RSI := FCloseRSI;
  xÑriterion.ReActiveRSI := FCloseActiveRSI;
  xÑriterion.Qty := 1;
  xÑriterion.IsActive := True;
end;

procedure TWorkTraderThread.SetID(const Value: Integer);
begin
  FID := Value;
  FTrader.ID := Value;
end;

end.
