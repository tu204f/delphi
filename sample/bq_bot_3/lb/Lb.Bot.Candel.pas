unit Lb.Bot.Candel;

interface

{$I debug_volt.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Journal.Trading,
  Lb.CustomWorkBot;

type
  ///<summary>
  /// Рабочий бот
  ///</summary>
  ///<remarks>
  /// Укаждого бота свой набор заявок
  ///</remarks>
  TWorkBot = class(TCustomWorkBot)
  private
    FCloseTriling: Double;
  protected
    function GetInfoValue: String; override;
    procedure SetCloseCurrentPosition(const APosition: TJournalPosition); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>
    /// Новая свеча
    ///</summary>
    procedure TradingNewCandel; override;
    ///<summary>
    /// Событие обновления данных, платформы
    ///</summary>
    procedure TradingUpDataCandel(const ATradingPlatform: TTradingPlatform); override;
  public
    ///<summary>
    /// Дельта закрытие позиции, после открытие
    ///</summary>
    property CloseTriling: Double read FCloseTriling write FCloseTriling;
  end;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  System.DateUtils;

{ TWorkBot }

constructor TWorkBot.Create;
begin
  inherited;
  FCloseTriling := 10;
end;

destructor TWorkBot.Destroy;
begin
  inherited;
end;

function TWorkBot.GetInfoValue: String;
begin
  Result := 'tr_' + FCloseTriling.ToString;
end;

procedure TWorkBot.SetCloseCurrentPosition(const APosition: TJournalPosition);
begin
  inherited;
  if Assigned(APosition) then
    with APosition do
    begin
      RatesSL := 0.5;
      RatesTK := 3;
      //Triling := Self.CloseTriling;
    end;
end;

procedure TWorkBot.TradingNewCandel;

{$IFDEF DBG_TRADING_NEW_CANDEL}
  procedure _LogCandel(const AIndex: Integer; const ACandel: TCandel);
  begin
    TLogger.LogTree(3,'Candel:>> [' + AIndex.ToString + ']' + ACandel.GetToStr);
  end;
{$ENDIF}

  function _GetIsTranding: Integer;
  var
    xCandel: TCandel;
    i, Count, xCountTranding: Integer;
  begin
    xCountTranding := 0;
    Count := StateMarket.Candels.Count;
    if Count > 0 then
      for i := 0 to 1 do
      begin
        xCandel := StateMarket.Candels[i];
        case xCandel.TypeCandel of
          tcGreen: xCountTranding := xCountTranding + 1;
          tcRed: xCountTranding := xCountTranding - 1;
        end;
        {$IFDEF DBG_TRADING_NEW_CANDEL}
        _LogCandel(i,xCandel);
        {$ENDIF}
      end;
    Result := xCountTranding;
  end;

var
  xInd_IsTrand: Integer;
begin
  inherited TradingNewCandel;
  {$IFDEF DBG_TRADING_NEW_CANDEL}
  TLogger.LogTree(0,'TWorkBot.SetTradingNewCandel');
  {$ENDIF}
  if Assigned(StateMarket) then
  begin
    if (StateMarket.Candels.Count > 0) and (StateMarket.Ask > 0) and (StateMarket.Bid > 0) then
    begin
      xInd_IsTrand := _GetIsTranding;
      {$IFDEF DBG_TRADING_NEW_CANDEL}
      TLogger.LogTree(3,'Tranding: >> ' + xInd_IsTrand.ToString);
      {$ENDIF}
      case xInd_IsTrand of
        2 : OpenPositionBuy;
        -2: OpenPositionSell;
      end;
    end;
  end;
end;

procedure TWorkBot.TradingUpDataCandel(const ATradingPlatform: TTradingPlatform);
begin
  inherited TradingUpDataCandel(ATradingPlatform);
end;

end.
