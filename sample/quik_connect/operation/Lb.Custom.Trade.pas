unit Lb.Custom.Trade;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Level;

type
  ///<summary>Торговые стратегии</summary>
  ///<remarks>Уже должна быть привязка к обнов инструменту</remarks>
  TCustomTrade = class(TObject)
  private
    FMinStep: Double;
    FReferenceLevel: TLevel;
  protected
    property ReferenceLevel: TLevel read FReferenceLevel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Минимальный шаг цены</summary>
    property MinStep: Double read FMinStep write FMinStep;
    procedure SetReferenceSecCode(const AReferencePrice, ALast, ABid, AOffer: Double); virtual;
  public
    ///<summary>Отправялем сообщение о торговой операции</summary>
    ///<returns>Возращается номер транзации</returns>
    function GetOrder(APrice: Double; AQuantity: Integer; ABuySell: Char): Integer; virtual;
    ///<summary>Отправляем приказ насянтие заявки</summary>
    function GetDeleteOrder(AOrderNo: Int64): Boolean; virtual;
    ///<summary>Обновление условие заявик</summary>
    function GetMoveOrder(AOrderNo: Int64; APrice: Double; AQuantity: Integer; ABuySell: Char): Integer; virtual;
  end;

implementation

{ TCustomTrade }

constructor TCustomTrade.Create;
begin
  FReferenceLevel := TLevel.Create;
end;

destructor TCustomTrade.Destroy;
begin
  FreeAndNil(FReferenceLevel);
  inherited;
end;

procedure TCustomTrade.SetReferenceSecCode(const AReferencePrice, ALast, ABid,
  AOffer: Double);
begin
  {реализуется торговая стратегия}
end;

function TCustomTrade.GetOrder(APrice: Double; AQuantity: Integer; ABuySell: Char): Integer;
begin
  Result := -1;
  {Выставить заявку}
end;

function TCustomTrade.GetDeleteOrder(AOrderNo: Int64): Boolean;
begin
  Result := False;
  {Удаление заявки из систмы}
end;

function TCustomTrade.GetMoveOrder(AOrderNo: Int64; APrice: Double; AQuantity: Integer; ABuySell: Char): Integer;
begin
  {Обновление заявки}
end;

end.
