unit Lb.Trade.RSI;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  ///<summary>Условия торговый операций</summary>
  TTrade = class(TObject)
  private
    FID           : Integer;
    FValueRSI     : Double;
    FActiveRSI    : Double;
    FIsAutoTrade  : Boolean;
    FIsActiveOrder: Boolean;
    FBuySell      : Char;
    FOnEventChage: TNotifyEvent;
  protected
    procedure DoEventChage;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ID: Integer read FID;
  public
    ///<summary>Значение активации заявки</summary>
    property ValueRSI: Double read FValueRSI write FValueRSI;
    ///<summary>Значение возобновление</summary>
    property ActiveRSI: Double read FActiveRSI write FActiveRSI;
    ///<summary>Отслеживаем занчение RSI</summary>
    property IsAutoTrade: Boolean read FIsAutoTrade write FIsAutoTrade;
    ///<summary>Возобновление активации RSI</summary>
    property IsActiveOrder: Boolean read FIsActiveOrder write FIsActiveOrder;
    ///<summary>Напровление сделки</summary>
    property BuySell: Char read FBuySell write FBuySell;
  public
    procedure UpDataValue(const AValue: Double);
    property OnEventChage: TNotifyEvent write FOnEventChage;
  end;

implementation

var
  localID: Integer = 0;

function GetID: Integer;
begin
  localID := localID + 1;
  Result := localID;
end;

{ TTrade }

constructor TTrade.Create;
begin
  FID := GetID;
end;

destructor TTrade.Destroy;
begin

  inherited;
end;

procedure TTrade.DoEventChage;
begin
  if Assigned(FOnEventChage) then
    FOnEventChage(Self);
end;

procedure TTrade.UpDataValue(const AValue: Double);

  function _IsRSI(const AValue: Double): Boolean;
  begin
    Result := False;
    if AValue > 0 then
    begin
      case FBuySell of
        'B': Result := FValueRSI > AValue;
        'S': Result := FValueRSI < AValue;
      end;
    end;
  end;

  function _IsRSI_2(const AValue: Double): Boolean;
  begin
    Result := False;
    if AValue > 0 then
    begin
      case FBuySell of
        'B': Result := FActiveRSI < AValue;
        'S': Result := FActiveRSI > AValue;
      end;
    end;
  end;

begin
  if FIsAutoTrade then
  begin
    if _IsRSI(AValue) then
    begin
      DoEventChage;
      FIsAutoTrade := False;
    end;
  end
  else
  begin
    // Когда галочка номер 2 (CheckBoxAuto) стоит
    // нужно включить галочку номерно 1 (CheckBoxActiveOrder)
    if FIsActiveOrder then
      FIsAutoTrade := _IsRSI_2(AValue);
  end;
end;

end.
