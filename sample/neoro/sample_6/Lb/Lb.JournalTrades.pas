unit Lb.JournalTrades;

interface

{$IFDEF DEBUG}
//  {$DEFINE DBL}
{$ENDIF}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel,
  Lb.Block;

type
  ///<summary>������</summary>
  TTrade = class(TObject)
  private
    FIsActive  : Boolean; // �������� ������
    FPriceOpen : Double;  // ���� �������� ������
    FPriceClose: Double;  // ���� �������� ������
    FTakeProfit: Double;  // ���� tale profit
    FStopLoss  : Double;  // ���� stop loss
    FBuySell   : Char;    // ����������� ������
    FQuantity  : Integer; // ����������
  protected

  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open(const APrice: Double; const AQuantity: Integer; const ABuySell: Char);
    procedure Close(const APrice: Double);
    procedure UpDate(const ACandel: TCandel);
  public
    ///<summary>�������� ������</summary>
    property IsActive  : Boolean read FIsActive;
    ///<summary>���� �������� ������</summary>
    property PriceOpen : Double read FPriceOpen;
    ///<summary>���� �������� ������</summary>
    property PriceClose: Double read FPriceClose;
    ///<summary>���� tale profit</summary>
    property TakeProfit: Double read FTakeProfit write FTakeProfit;
    ///<summary>���� stop loss</summary>
    property StopLoss  : Double read FStopLoss write FStopLoss;
    ///<summary>����������� ������</summary>
    property BuySell   : Char read FBuySell;
    ///<summary>����������</summary>
    property Quantity  : Integer read FQuantity;
    ///<summary>������</summary>
    function GetProfit: Double;
  public
    function GetTakeProfitPrice: Double;
    function GetStopLossPrice: Double;
  end;

  ///<summary>������ ������</summary>
  TTradeList = TObjectList<TTrade>;

///<summary>�������� ��������� �����������</summary>
function GetRandomTypeDecision: TTypeDecision;

implementation

{$IFDEF DBL}
uses
  Lb.Logger;
{$ENDIF}

function GetRandomTypeDecision: TTypeDecision;
var
  xV1, xV2: Double;
begin
  Randomize;
  Result := TTypeDecision.tdWait;
  if Random < 0.01 then
  begin
    xV1 := Random;
    xV2 := Random;
    if xV1 > xV2 then
      Result := TTypeDecision.tdBuy
    else
      Result := TTypeDecision.tdSell;
  end;
end;

{ TTrade }

constructor TTrade.Create;
begin
  FIsActive := False;
end;

destructor TTrade.Destroy;
begin

  inherited;
end;

procedure TTrade.UpDate(const ACandel: TCandel);

  function _GetIntersectionUp(const APrice: Double; const ACandel: TCandel): Double;
  begin
    // ����������� ���� ������ ����
    Result := -1;
    if (ACandel.Open > APrice) and (ACandel.Low <= APrice) then
      Result := APrice
    else if (ACandel.Open <= APrice) then
      Result := ACandel.Open;
    {$IFDEF DBL}
    if Result > 0 then
    begin
      TLogger.LogTree(0,'_GetIntersectionUp:');
      TLogger.LogTreeText(3,'>> Price: ' + APrice.ToString);
      TLogger.Log('');
      TLogger.LogTreeText(3,'>> Open : ' + ACandel.Open.ToString);
      TLogger.LogTreeText(3,'>> High : ' + ACandel.High.ToString);
      TLogger.LogTreeText(3,'>> Low  : ' + ACandel.Low.ToString);
      TLogger.LogTreeText(3,'>> Close: ' + ACandel.Close.ToString);
    end;
    {$ENDIF}
  end;

  function _GetIntersectionDown(const APrice: Double; const ACandel: TCandel): Double;
  begin
    // �����������  ���� ����� ����
    Result := -1;
    if (ACandel.Open < APrice) and (ACandel.High >= APrice) then
      Result := APrice
    else if (ACandel.Open >= APrice) then
      Result := ACandel.Open;
    {$IFDEF DBL}
    if Result > 0 then
    begin
      TLogger.LogTree(0,'_GetIntersectionDown:');
      TLogger.LogTreeText(3,'>> Price: ' + APrice.ToString);
      TLogger.Log('');
      TLogger.LogTreeText(3,'>> Open : ' + ACandel.Open.ToString);
      TLogger.LogTreeText(3,'>> High : ' + ACandel.High.ToString);
      TLogger.LogTreeText(3,'>> Low  : ' + ACandel.Low.ToString);
      TLogger.LogTreeText(3,'>> Close: ' + ACandel.Close.ToString);
    end;
    {$ENDIF}
  end;

var
  xPrice: Double;
  xTP, xSL: Double;
begin
  // ��������� ������� �������� �������
  if FIsActive then
  begin
    xTP := GetTakeProfitPrice;
    xSL := GetStopLossPrice;
    case FBuySell of
      'B': begin
        // -------------------------
        // �������� �� �������
        // ����������� ���� ����� � ����
        xPrice := _GetIntersectionDown(xTP,ACandel);
        if xPrice > 0 then
        begin
          {$IFDEF DBL}
          TLogger.Log('�������� �� ��� �������: ' + xPrice.ToString);
          {$ENDIF}
          Self.Close(xPrice);
        end;
        // -------------------------
        // �������� �� stop loss
        // ����������� ���� ������ � ����
        xPrice := _GetIntersectionUp(xSL,ACandel);
        if xPrice > 0 then
        begin
          {$IFDEF DBL}
          TLogger.Log('�������� �� ���� ����: ' + xPrice.ToString);
          {$ENDIF}
          Self.Close(xPrice);
        end;
      end;
      'S': begin
        // -------------------------
        // �������� �� �������
        // ����������� ���� � ����� ����
        xPrice := _GetIntersectionUp(xTP,ACandel);
        if xPrice > 0 then
        begin
          {$IFDEF DBL}
          TLogger.Log('�������� �� ��� �������: ' + xPrice.ToString);
          {$ENDIF}
          Self.Close(xPrice);
        end;
        // -------------------------
        // �������� �� stop loss
        // ����������� ���� ����� � ����
        xPrice := _GetIntersectionDown(xSL,ACandel);
        if xPrice > 0 then
        begin
          {$IFDEF DBL}
          TLogger.Log('�������� �� ���� ����: ' + xPrice.ToString);
          {$ENDIF}
          Self.Close(xPrice);
        end
      end;
    end;
  end;
end;

procedure TTrade.Open(const APrice: Double; const AQuantity: Integer;
  const ABuySell: Char);
begin
  if not FIsActive then
  begin
    FIsActive    := True;
    FPriceOpen   := APrice;
    FQuantity    := AQuantity;
    FBuySell     := ABuySell;
    {$IFDEF DBL}
    TLogger.LogText('#',80);
    TLogger.LogTree(0,'�������� �������:');
    TLogger.LogTreeText(3,'>> PriceOpen: ' + FPriceOpen.ToString);
    TLogger.LogTreeText(3,'>> Quantity : ' + FQuantity.ToString);
    TLogger.LogTreeText(3,'>> BuySell  : ' + FBuySell);
    {$ENDIF}
  end;
end;

procedure TTrade.Close(const APrice: Double);
begin
  if FIsActive then
  begin
    FPriceClose := APrice;
    FIsActive := False;
    {$IFDEF DBL}
    TLogger.LogTree(0,'�������� �������:');
    TLogger.LogTreeText(3,'>> PriceClose: ' + FPriceClose.ToString);
    {$ENDIF}
  end;
end;

function TTrade.GetTakeProfitPrice: Double;
begin
  case FBuySell of
    'B': Result := FPriceOpen * (1 + FTakeProfit/100);
    'S': Result := FPriceOpen * (1 - FTakeProfit/100);
  end;
end;

function TTrade.GetProfit: Double;
begin
  case FBuySell of
    'B': Result := (FPriceClose - FPriceOpen)  * FQuantity;
    'S': Result := (FPriceOpen  - FPriceClose) * FQuantity;
  end;
end;

function TTrade.GetStopLossPrice: Double;
begin
  case FBuySell of
    'B': Result := FPriceOpen * (1 - FStopLoss/100);
    'S': Result := FPriceOpen * (1 + FStopLoss/100);
  end;
end;

end.
