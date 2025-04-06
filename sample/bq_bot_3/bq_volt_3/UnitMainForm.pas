unit UnitMainForm;

interface

{$I debug_volt.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,

  Lb.Indicator,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Platform.Bybit,
  Lb.Journal.Trading,
  Lb.Bot.V4,

  FMX.Objects,
  FMX.Layouts,
  FMX.TabControl,
  FMX.Menus,
  FMX.Edit,
  FMXTee.Engine,
  FMXTee.Series,
  FMXTee.Procs,
  FMXTee.Chart,
  FMX.ListBox,
  FMX.Memo.Types,
  FMX.Memo,

  UnitWorkBotPanelFrame,
  UnitWorkBotFrame;

type
  TMainForm = class(TForm, IMainFormLog)
    ButtonStartOrStop: TButton;
    Rectangle: TRectangle;
    TextStatus: TText;
    TabControl1: TTabControl;
    TabItemTrade1: TTabItem;
    TabItemPosition: TTabItem;
    PopupMenu: TPopupMenu;
    MenuItemSaveFile: TMenuItem;
    StringGridCandel: TStringGrid;
    MemoLog: TMemo;
    TabItemTrade2: TTabItem;
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
    procedure TradingPlatformOnNewCandel(Sender: TObject);
    procedure TradingPlatformOnMsgInfo(ASender: TObject; AMsg: String);
  protected
    procedure DoStart;
    procedure DoStop;
    procedure LogMsg(const S: WideString);
  public
    WorkBotPanelFrame: TWorkBotPanelFrame;
    WorkBotPanelReversFrame: TWorkBotPanelFrame;
    TradingPlatform: TTradingPlatform;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  System.DateUtils;


constructor TMainForm.Create(AOwner: TComponent);

  procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := AStrGrid;
    xCol.Header := AHeader;
    xCol.Width  := AWidth;
  end;

  procedure SetShowStringGridCandel;
  begin
    SetAddColumn(StringGridCandel,'Time',150);
    SetAddColumn(StringGridCandel,'Open');
    SetAddColumn(StringGridCandel,'High');
    SetAddColumn(StringGridCandel,'Low');
    SetAddColumn(StringGridCandel,'Close');
    SetAddColumn(StringGridCandel,'Vol');
  end;

begin
  inherited;
  SetShowStringGridCandel;

  // *************************************************************************
  // Реализация торговой платформы
  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;
  TradingPlatform.OnNewCandel   := TradingPlatformOnNewCandel;
  TradingPlatform.OnMsgInfo     := TradingPlatformOnMsgInfo;
  // 'DdncwwQY6AVdShL008';
  // 'ldfYDnYhlVU5SU7w89mOnaHi0icy8XctNXtT';
  TPlatfomBybit(TradingPlatform).ApiKey := 't0YI4Ou0TKOTd7WrkE';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_60;


  // *************************************************************************
  // Торговая панель
  WorkBotPanelFrame := TWorkBotPanelFrame.Create(nil);
  WorkBotPanelFrame.TradingPlatform := TradingPlatform;
  WorkBotPanelFrame.Parent := TabItemTrade1;
  WorkBotPanelFrame.Align := TAlignLayout.Client;
  WorkBotPanelFrame.MainFormLog := Self;
  WorkBotPanelFrame.WorkBot.Rate := 0.2;
  WorkBotPanelFrame.WorkBot.IsRevers := False;

  // ************************************************************************
  // Обратная торговая стратегия
  WorkBotPanelReversFrame := TWorkBotPanelFrame.Create(nil);
  WorkBotPanelReversFrame.TradingPlatform := TradingPlatform;
  WorkBotPanelReversFrame.Parent := TabItemTrade2;
  WorkBotPanelReversFrame.Align := TAlignLayout.Client;
  WorkBotPanelReversFrame.MainFormLog := Self;
  WorkBotPanelReversFrame.WorkBot.Rate := 0.2;
  WorkBotPanelReversFrame.WorkBot.IsRevers := True;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradingPlatform);
  inherited;
end;

procedure TMainForm.LogMsg(const S: WideString);
var
  xS: String;
begin
  xS := FormatDateTime('hh:nn:ss.zzz',Time) + '| ' + S;
  MemoLog.Lines.Add(xS);
end;

procedure TMainForm.DoStart;
begin
  if not TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Стоп';
    TradingPlatform.Symbol := 'BTCUSDT';
    TradingPlatform.StateMarket.Qty := 10;
    TradingPlatform.Start;
  end;
end;

procedure TMainForm.DoStop;
begin
  if TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Старт';
    TradingPlatform.Stop;
  end;
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Пробитие волатильности';
end;

procedure TMainForm.ButtonStartOrStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    DoStop
  else
    DoStart;
end;

procedure TMainForm.TradingPlatformOnMsgInfo(ASender: TObject; AMsg: String);
begin
  {todo: сообщение работы}
  LogMsg('MsgInfo: ' + AMsg);
end;

procedure TMainForm.TradingPlatformOnNewCandel(Sender: TObject);
begin
  {todo: новая свеча}
  WorkBotPanelFrame.TradingPlatformNewCandel;
  WorkBotPanelReversFrame.TradingPlatformNewCandel;
end;

procedure TMainForm.TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);

  procedure _ShowCandel;
  var
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    iCount := AStateMarket.Candels.Count;
    StringGridCandel.RowCount := iCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := AStateMarket.Candels[i];
        StringGridCandel.Cells[0,i] := DateTimeToStr(UnixToDateTime(xCandel.Time));
        StringGridCandel.Cells[1,i] := xCandel.Open.ToString;
        StringGridCandel.Cells[2,i] := xCandel.High.ToString;
        StringGridCandel.Cells[3,i] := xCandel.Low.ToString;
        StringGridCandel.Cells[4,i] := xCandel.Close.ToString;
        StringGridCandel.Cells[5,i] := xCandel.Vol.ToString;
      end;
  end;

  function _ToBytes(S: String): TBytes;
  var
    xC: Char;
    xB: TBytes;
    xLength, i: Integer;
  begin
    xLength := S.Length;
    SetLength(xB,xLength);

    i := 0;
    while i < xLength do
    begin
      xC := S[i + 1];
      xB[i] := Ord(xC);
      i := i + 1;
    end;
    Result := xB;
  end;

  procedure _SaveTakt(const AAsk, ABid, ADeviationValue: Double; ACandel: TCandel);
  var
    xS: String;
    xB: TBytes;
    xPath: String;
    xFS: TFileStream;
  begin

    xS := AAsk.ToString + ';' + ABid.ToString + ';' + ADeviationValue.ToString + ';';
    xS := xS + ACandel.Time.ToString + ';';
    xS := xS + DateTimeToStr(UnixToDateTime(ACandel.Time)) + ';';
    xS := xS + ACandel.Open.ToString + ';';
    xS := xS + ACandel.High.ToString + ';';
    xS := xS + ACandel.Low.ToString + ';';
    xS := xS + ACandel.Close.ToString + ';';
    xS := xS + ACandel.Vol.ToString + ';';
    xS := xS + 'endl' + sLineBreak;

    xPath := ExtractFilePath(ParamStr(0)) + 'history.txt';
    if FileExists(xPath) then
      xFS := TFileStream.Create(xPath,fmOpenWrite)
    else
      xFS := TFileStream.Create(xPath,fmCreate);
    try
      xFS.Position := xFS.Size;
      xB := _ToBytes(xS);
      xFS.Write(xB,Length(xB));
    finally
      FreeAndNil(xFS);
    end;
  end;

begin
  // **************************************************************************
  // Оценка состояния рынка
  TextStatus.Text :=
    'Price: ' +
    TradingPlatform.StateMarket.Ask.ToString + '/' +
    TradingPlatform.StateMarket.Bid.ToString + ';';

  if AStateMarket.Candels.Count > 0 then
  begin
    _SaveTakt(
      AStateMarket.Ask,
      AStateMarket.Bid,
      TradingPlatform.ValueVolatility.DeviationValue,
      AStateMarket.Candels.FirstCandel
    );
  end;


  // *************************************************************************
  // Исторические данные
  _ShowCandel;

{$IFDEF DBG_TRADING}
  TLogger.LogText('*',80);
  TLogger.LogTree(0,'BEGIN.TradingPlatform');
{$ENDIF}
  WorkBotPanelFrame.TradingPlatformStateMarket(AStateMarket);
  WorkBotPanelReversFrame.TradingPlatformStateMarket(AStateMarket);
{$IFDEF DBG_TRADING}
  TLogger.LogTree(0,'END.TradingPlatform');
{$ENDIF}
end;


end.
