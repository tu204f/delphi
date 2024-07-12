unit Lb.Trader;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
  Lb.Bybit.SysUtils;

type
  TOrder = class;

  ///<summary>
  /// Тип категорий
  ///</summary>
  TTypeСriterion = (tcOpen, tcClose);

  ///<summary>
  /// Критерий отрытие позиции
  ///</summary>
  TСriterion = class(TObject)
  public
    ///<summary>
    /// Направление критерия
    ///</summary>
    Side: TTypeSide;

    ///<summary>
    /// Значение индикатора
    ///</summary>
    RSI: Double;

    ///<summary>
    /// Значение индикатора, для активации кретерия
    ///</summary>
    ReActiveRSI: Double;

    ///<summary>
    /// Количество
    ///</summary>
    Qty: Double;

    ///<summary>
    /// Активировать активировать
    ///</summary>
    IsActive: Boolean;

    ///<summary>
    /// Тип критерия: Открытие, Закрытие
    ///</summary>
    TypeСriterion: TTypeСriterion;
  public
    constructor Create; virtual;
    procedure Default;
  end;
  TСriterionList = TObjectList<TСriterion>;

  ///<summary>Событие отправки на действие</summary>
  TOrder = class(TObject)
  private

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

///<summary>
/// Пересечение значение RSI,
/// когда текущие значение (AValue),
/// пересекает (AParam) - становится больше
///</summary>
function GetUpRSI(const AValue, AParam: Double): Boolean;
begin
  Result := AParam < AValue;
end;

///<summary>
/// Пересечение значение RSI,
/// когда текущие значение (AValue),
/// пересекает (AParam) - становиться меньше
///</summary>
function GetDownRSI(const AValue, AParam: Double): Boolean;
begin
  Result := AParam > AValue;
end;

{ TСriterion }

constructor TСriterion.Create;
begin

end;

procedure TСriterion.Default;
begin

end;

{ TOrder }

constructor TOrder.Create;
begin

end;

destructor TOrder.Destroy;
begin

  inherited;
end;



end.
