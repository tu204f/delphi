(*******************************************************************************
  Версия торговли в коридоре, с фиксирваным значением, без оценки влатиности
*******************************************************************************)
unit Lb.Operation.V1;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Custom.Trade,
  Lb.Level;

type
  ///<summary>Торговые стратегии</summary>
  TOperationTrade = class(TCustomTrade)
  private
    FStepPrice: Integer;
    FBuyLevel: TLevel;
    FSellLevel: TLevel;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Количество шагов</summary>
    property StepPrice: Integer write FStepPrice;
    procedure SetReferenceSecCode(const AReferencePrice, ALast, ABid, AOffer: Double); override;
  public
    property BuyLevel: TLevel read FBuyLevel;
    property SellLevel: TLevel read FSellLevel;
  end;

implementation

{ TOperationTrade }

constructor TOperationTrade.Create;
begin
  inherited;
  FStepPrice := 10;
  FBuyLevel := TLevel.Create;
  FSellLevel := TLevel.Create;
end;

destructor TOperationTrade.Destroy;
begin
  FreeAndNil(FSellLevel);
  FreeAndNil(FBuyLevel);
  inherited;
end;

procedure TOperationTrade.SetReferenceSecCode(const AReferencePrice, ALast, ABid, AOffer: Double);
begin
  if Self.MinStep <= 0 then
    Exit;
  var xReferencePrice := Trunc(AReferencePrice/Self.MinStep) * Self.MinStep;
  Self.ReferenceLevel.Price := xReferencePrice;
  Self.BuyLevel.Price := xReferencePrice - FStepPrice * Self.MinStep;
  Self.SellLevel.Price := xReferencePrice + FStepPrice * Self.MinStep;
  if (AReferencePrice > 0) then
  begin
    if Self.BuyLevel.Price < ALast then
    begin
      // Обновляем условие заявки на покупку

    end;
    if Self.SellLevel.Price > ALast then
    begin
      // Обновляем условие заявки на продажу

    end;
  end;
end;

end.
