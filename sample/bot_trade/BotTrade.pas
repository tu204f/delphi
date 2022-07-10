unit BotTrade;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Candel.SysUtils;

type
  ///<summary>Напровление принятого решение</summary>
  ///<param name="tsNull">нет решение</param>
  ///<param name="tsBuy">Открыть длиную позицию</param>
  ///<param name="tsSell">Открыть короткую позицию</param>
  TTypeDirection = (
    tsNull,
    tsBuy,
    tsSell
  );

  ///<summary>Состояние позиции</summary>
  TTypeStatusPosition = (
    tspNull,
    tspOpen,
    tspClose
  );

  ///<summary>Проводимая операция</summary>
  TTradePosition = record
    ///<summary>Дата открытие позции</summary>
    OpenDate: TDate;
    ///<summary>Время открытие позции</summary>
    OpenTime: TTime;
    CloseDate: TDate;
    CloseTime: TTime;
    ///<summary>Цена открытия</summary>
    Open: Double;
    ///<summary>Цена закрытие</summary>
    Close: Double;
    ///<summary>Объем позиции</summary>
    Quantity: Integer;
    ///<summary>Дериктива позиции</summary>
    Direction: TTypeDirection;
    ///<summary>Состоние позиции</summary>
    Status: TTypeStatusPosition;

    ///<symmary>Ожидаемая прибыть позиции</summary>
    Profit: Double;
    ///<symmary></Ожидаемый убыток позиции>
    Stop: Double;
  public
    function GetProfitPerUnit: Double;
  end;

  ///<summary>Список позиции</summary>
  TTradePositionList = TList<TTradePosition>;

  {todo: прописать объем операции, Quantity в инициализацию}

  TCustomBot = class(TObject)
  private
    FIsInit: Boolean;
    FValueProfit,
    FValueStop: Double;
    FPositions: TTradePositionList;
    FPosition: TTradePosition;
  protected
    procedure OpenPosition; virtual;
    procedure ClosePosition(const AForcedStop: Boolean = False); virtual;
    ///<summary>Решение которые принимает робот</summary>
    ///<remarks>
    ///   Реализация торгового робота, на другой стратегии
    ///   Достаточное ее парализовать
    ///</remarks>
    function GetSolutionOpenPosition: TTypeDirection; virtual;
    ///<summary>Процедура принятие решение</summary>
    ///<remarks>Принятое решение записывается в FPositions</remarks>
    procedure DoSolution; virtual;

    property IsInit: Boolean read FIsInit;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Передаем начальные значение</summary>
    ///<param name="AValueProfit">Размер профита, который нужно фиксировать</param>
    ///<param name="AValueStop">Размер убытка, который нужно фиксировать</param>
    procedure Init(const AValueProfit, AValueStop: Double);
    property Positions: TTradePositionList read FPositions;
    property Position: TTradePosition read FPosition;

    property ValueProfit: Double read FValueProfit;
    property ValueStop: Double read FValueStop;
  end;

  ///<summary>Торговый роботы: робот с генератом случайного числа</summary>
  TDefaultBot = class(TCustomBot)
  private
    FID: Integer;
    FValueTypeDirection: TTypeDirection;
  private
    FMaxPosition, FMinPosition: Double;
    FCandels: TCandelList;
    function GetCandelLast: TCandel;
  protected
    procedure OpenPosition; override;
    procedure ClosePosition(const AForcedStop: Boolean = False); override;
    function GetSolutionOpenPosition: TTypeDirection; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Перезапуск бота</summary>
    procedure ExecutionStart;
    ///<summary>Последния свеча</summary>
    procedure ExecutionLastCandel(const ACandel: TCandel);
    ///<summary>Принудительная остановка</summary>
    procedure ExecutionStop;
    ///<summary>Свечи</summary>
    property Candels: TCandelList read FCandels;
    property ID: Integer read FID write FID;
    ///<summary>Размер полученого профита</summary>
    function GetProfitResult: Double;

    property MaxPosition: Double read FMaxPosition;
    property MinPosition: Double read FMinPosition;

  end;

  TTechAnalit = record
    class function GetRoc(const APriceLast, APriceN: Double): Double; static;
  end;

function GetDirectionToStr(Value: TTypeDirection): String;
function GetStatusToStr(Value: TTypeStatusPosition): String;

implementation

uses
   System.Math;

function GetDirectionToStr(Value: TTypeDirection): String;
begin
  case Value of
    tsNull: Result := 'null';
    tsBuy: Result := 'buy';
    tsSell: Result := 'sell';
  end;
end;

function GetStatusToStr(Value: TTypeStatusPosition): String;
begin
  case Value of
    tspNull: Result := 'null';
    tspOpen: Result := 'open';
    tspClose: Result := 'close';
  end;
end;

{ TTradePosition }

function TTradePosition.GetProfitPerUnit: Double;
begin
  case Direction of
    tsBuy: Result := Close - Open;
    tsSell: Result := Open - Close;
  else
    Result := 0;
  end;
end;

{ TCustomBot }

constructor TCustomBot.Create;
begin
  FIsInit := False;
  FPositions := TTradePositionList.Create;
end;

destructor TCustomBot.Destroy;
begin
  FreeAndNil(FPositions);
  inherited;
end;

procedure TCustomBot.OpenPosition;
begin
  {открываем позицию}
end;

procedure TCustomBot.ClosePosition(const AForcedStop: Boolean);
begin
  {закрываем позицию}
end;

procedure TCustomBot.DoSolution;
begin
  if FPosition.Status = tspClose then
    FillChar(FPosition,SizeOf(FPosition),0);
  case FPosition.Status of
    tspNull: OpenPosition;
    tspOpen: ClosePosition;
  end;
end;

function TCustomBot.GetSolutionOpenPosition: TTypeDirection;
begin
  Result := TTypeDirection.tsNull;
end;

procedure TCustomBot.Init(const AValueProfit, AValueStop: Double);
begin
  FIsInit := True;
  FValueProfit := AValueProfit;
  FValueStop := AValueStop;
  FPositions.Clear;
end;

{ TDefaultBot }

constructor TDefaultBot.Create;
begin
  inherited Create;
  FValueTypeDirection := tsNull;
  FCandels := TCandelList.Create;
end;

destructor TDefaultBot.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

procedure TDefaultBot.ExecutionLastCandel(const ACandel: TCandel);
begin
  if not IsInit then
    raise Exception.Create('Error Message: Данные робота не были проинициализированы');

  FCandels.Add(ACandel);
  DoSolution;

  if FPosition.Status = tspOpen then
    FPosition.Close := ACandel.Close;

end;

function TDefaultBot.GetSolutionOpenPosition: TTypeDirection;
begin
  //Randomize;
  //var xValue := RandomRange(0,2);
  case FValueTypeDirection of
    tsNull: FValueTypeDirection := tsBuy;
    tsBuy: FValueTypeDirection := tsSell;
    tsSell: FValueTypeDirection := tsBuy;
  end;
  Result := FValueTypeDirection;
end;

procedure TDefaultBot.ExecutionStart;
begin
  FMaxPosition := 0;
  FMinPosition := 0;
  FPositions.Clear;
end;

procedure TDefaultBot.ExecutionStop;
begin
  // Принудительно закрываем позицию
  Self.ClosePosition(True);
end;

function TDefaultBot.GetCandelLast: TCandel;
begin
  if FCandels.Count > 0 then
    Result := FCandels[FCandels.Count - 1]
  else
    raise Exception.Create('Error Message: Нет не одной свечи');
end;

procedure TDefaultBot.OpenPosition;
var
  xValue: TTypeDirection;
  xCandel: TCandel;
begin
  xValue := GetSolutionOpenPosition;
  if xValue in [tsBuy,tsSell] then
  begin
    xCandel := GetCandelLast;
    FPosition.OpenDate := xCandel.Date;
    FPosition.OpenTime := xCandel.Time;
    FPosition.Open := xCandel.Close;
    case xValue of
      tsBuy: begin
        FPosition.Profit := FPosition.Open + FValueProfit;
        FPosition.Stop   := FPosition.Open - FValueStop;
      end;
      tsSell: begin
        FPosition.Profit := FPosition.Open - FValueProfit;
        FPosition.Stop   := FPosition.Open + FValueStop;
      end;
    end;
    FPosition.Quantity := 1;
    FPosition.Direction := xValue;
    FPosition.Status := TTypeStatusPosition.tspOpen;
  end;
end;

procedure TDefaultBot.ClosePosition(const AForcedStop: Boolean);


  function _GetProfit(const ACandel: TCandel): Boolean;
  begin
    case FPosition.Direction of
      tsBuy : Result := ACandel.High >= FPosition.Profit;
      tsSell: Result := ACandel.Low  <= FPosition.Profit;
    else
      Result := False;
    end;
  end;

  function _GetStop(const ACandel: TCandel): Boolean;
  begin
    case FPosition.Direction of
      tsBuy : Result := ACandel.Low   <= FPosition.Stop;
      tsSell: Result := ACandel.High  >= FPosition.Stop;
    else
      Result := False;
    end;
  end;

var
  xCandel: TCandel;
begin

  if FCandels.Count = 0 then
     Exit;


  xCandel := GetCandelLast;
  if AForcedStop then
  begin
    FPosition.CloseDate := xCandel.Date;
    FPosition.CloseTime := xCandel.Time;
    FPosition.Close := xCandel.Close;
    FPosition.Status := tspClose;
    FPositions.Add(FPosition);
    FCandels.Clear;
    FillChar(FPosition,SIzeOf(FPosition),0);
  end
  else if _GetProfit(xCandel) then
  begin
    FPosition.CloseDate := xCandel.Date;
    FPosition.CloseTime := xCandel.Time;
    FPosition.Close := FPosition.Profit;
    FPosition.Status := tspClose;
    FPositions.Add(FPosition);
    FCandels.Clear;
    FillChar(FPosition,SIzeOf(FPosition),0);
  end
  else if _GetStop(xCandel) then
  begin
    FPosition.CloseDate := xCandel.Date;
    FPosition.CloseTime := xCandel.Time;
    FPosition.Close := FPosition.Stop;
    FPosition.Status := tspClose;
    FPositions.Add(FPosition);
    FCandels.Clear;
    FillChar(FPosition,SIzeOf(FPosition),0);
  end;
end;

function TDefaultBot.GetProfitResult: Double;
var
  xSum: Double;
  xPosition: TTradePosition;
begin
  xSum := 0;
  for xPosition in FPositions do
    xSum := xSum + xPosition.GetProfitPerUnit;
  Result := xSum;

  if FMaxPosition < xSum then
    FMaxPosition := xSum;

  if FMinPosition = 0 then
    FMinPosition := xSum
  else if FMinPosition > xSum then
    FMinPosition := xSum;

end;


{ TTechAnalit }

class function TTechAnalit.GetRoc(const APriceLast, APriceN: Double): Double;
begin
  if APriceN > 0 then
    Result := 100 *((APriceLast - APriceN)/APriceN)
  else
    raise Exception.Create('Error Message: APriceN = ' + FloatToStr(APriceN) +
      '. Значение должно быть больше нуля.' );
end;

end.
