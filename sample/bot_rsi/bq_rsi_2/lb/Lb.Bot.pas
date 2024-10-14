unit Lb.Bot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils,
  Lb.Platfom;

type


  ///<summary>
  /// Бот - для торговли
  ///</summary>
  TBot = class(TObject)
  private
    FTradingPlatform: TTradingPlatform;
  protected
    ///<summary>
    /// Проверка возможности совершение торговых операций
    ///</summary>
    function IsTrading: Boolean;
    ///<summary>
    /// Есть активня позиция
    ///</summary>
    function IsActivePosition: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetSelected(const ACandels: TCandelList);
    ///<summary>
    /// На какой платформе работает — бот программы
    ///</summary>
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
  end;

implementation

{ TBot }

constructor TBot.Create;
begin
  FTradingPlatform := nil;
end;

destructor TBot.Destroy;
begin

  inherited;
end;

function TBot.IsTrading: Boolean;
begin
  // 1. Временной интервал, который можно торговать
  // 2. Размер полученного убытка
  // 3. Количество торговых операция — за торговый период
  // 4. Период заморозки торговли после получение убытка
  Result := True;
end;

procedure TBot.SetSelected(const ACandels: TCandelList);
begin
  if IsTrading then
  begin
    // Можно соверщать торговые операции
  end
  else if IsActivePosition then
  begin
    // Принудительно закрывать позицию
  end;
end;

end.
