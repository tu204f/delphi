unit Lb.CandelBot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants;

type
  ///<summary>
  ///Рассматривается ситуация когда бот работает по одному контракту
  ///</summary>
  TCandelBot = class(TObject)
  public type
    ///<summary>Сделка бота</summary>
    TTradeBot = class(TObject)
      Price: Double;
      Quantity: Integer;
      BuySell: Char;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
    end;

  private
    FOpenTrade: TTradeBot;
    FCloseTrade: TTradeBot;
  protected
    ///<summary>Открытие позиции</summary>
    procedure OpenTradePosition(const APrice: Double; const AQunaity: Integer; const ABuySell: Char);
    ///<summary>Закрытие позции</summary>
    procedure CloseTradePosition(const APrice: Double; const AQunaity: Integer; const ABuySell: Char);
  public
    constructor Create;
    destructor Destroy; override;
    property OpenTrade: TTradeBot read FOpenTrade;
    property CloseTrade: TTradeBot read FCloseTrade;
  end;

implementation

{ TCandelBot.TTradeBot }

constructor TCandelBot.TTradeBot.Create;
begin

end;

destructor TCandelBot.TTradeBot.Destroy;
begin

  inherited;
end;


procedure TCandelBot.TTradeBot.Clear;
begin
  Price := 0;
  Quantity := 0;
  BuySell := #0;
end;

{ TCandelBot }


constructor TCandelBot.Create;
begin
  FOpenTrade := TTradeBot.Create;
  FCloseTrade := TTradeBot.Create;
end;

destructor TCandelBot.Destroy;
begin
  FreeAndNil(FCloseTrade);
  FreeAndNil(FOpenTrade);
  inherited;
end;

procedure TCandelBot.OpenTradePosition(const APrice: Double; const AQunaity: Integer; const ABuySell: Char);
begin

end;

procedure TCandelBot.CloseTradePosition(const APrice: Double; const AQunaity: Integer; const ABuySell: Char);
begin

end;

end.
