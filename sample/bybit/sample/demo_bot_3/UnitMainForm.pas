unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  FMX.ComboEdit,
  FMX.StdCtrls,
  Lb.Bybit.SysUtils,
  Lb.HistoryIndicator,
  Lb.Bybit.InstrumentsInfo,
  Lb.Bybit.OrderBook,
  UnitIndicatorFrame,
  UnitBotFrame;

type
  TMainForm = class(TForm)
    LayoutMenu: TLayout;
    LayoutMain: TLayout;
    Layout: TLayout;
    ButtonStart: TButton;
    Timer: TTimer;
    LayoutLog: TLayout;
    MemoLog: TMemo;
    LayoutHistoryIndicator: TLayout;
    ComboEditSymbol: TComboEdit;
    LayoutBot: TLayout;
    ButtonInstrumentsInfo: TButton;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ComboBoxSymbolChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonInstrumentsInfoClick(Sender: TObject);
  private
    procedure SetStart;
    procedure SetStop;
  private {bybit - списо инструментов и придельные значение}
    InstrumentsInfo: TBybitInstrumentsInfo;
    procedure SelectedInstrumentsInfo;
    procedure InstrumentsInfoOnEventEndLoading(Sender: TObject);
  private {bybit - котировачный стакан}
    BybitOrderBook: TBybitOrderBook;
  public
    HistoryIndicator: THistoryIndicator;
    IndicatorFrame  : TIndicatorFrame;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Params,
  Lb.Setting;

var
  InstrumentInfo: TStringParams = nil;

{ TForm5 }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Загужаем история
  HistoryIndicator := THistoryIndicator.Create;

  // Фрем вывода
  IndicatorFrame := TIndicatorFrame.Create(Self);
  IndicatorFrame.Parent := LayoutHistoryIndicator;
  IndicatorFrame.Align := TAlignLayout.Client;

  // Информация инструменту
  InstrumentsInfo := TBybitInstrumentsInfo.Create;
  InstrumentsInfo.OnEventEndLoading := InstrumentsInfoOnEventEndLoading;

  InstrumentInfo := TStringParams.Create;
end;


destructor TMainForm.Destroy;
begin
  FreeAndNil(InstrumentInfo);
  FreeAndNil(InstrumentsInfo);
  FreeAndNil(IndicatorFrame);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  InstrumentInfo.Load('instrument');

  // ОПеределяем параметры
  {todo: параметры перевести под настройку проекта}
  HistoryIndicator.Symbol := InstrumentInfo.AsString['symbol'];
  HistoryIndicator.Category := TTypeCategory.tcLinear;
  HistoryIndicator.Interval := TTypeInterval.ti_5;
  HistoryIndicator.Period := 14;

  ComboEditSymbol.Text := InstrumentInfo.AsString['symbol'];

end;

procedure TMainForm.SelectedInstrumentsInfo;
begin
  InstrumentsInfo.Category := TTypeCategory.tcLinear;
  InstrumentsInfo.Status := TTypeStatus.tsTrading;
  InstrumentsInfo.Selected;
end;

procedure TMainForm.SetStart;
begin
  ButtonStart.Text := 'Стоп';
  HistoryIndicator.UpDate;
  if IndicatorFrame.SetUpDate(HistoryIndicator) then
    Timer.Enabled := True;
end;

procedure TMainForm.SetStop;
begin
  ButtonStart.Text := 'Старт';
  Timer.Enabled := False;
end;

procedure TMainForm.InstrumentsInfoOnEventEndLoading(Sender: TObject);
var
  xLinearObject: TLinearObject;
begin
  ComboEditSymbol.BeginUpdate;
  try
    ComboEditSymbol.Items.Clear;
    for xLinearObject in InstrumentsInfo.LinearObjects do
      ComboEditSymbol.Items.Add(xLinearObject.Symbol);
  finally
    ComboEditSymbol.EndUpdate;
  end;
end;

procedure TMainForm.ComboBoxSymbolChange(Sender: TObject);
var
  xItem: String;
  xIndex: Integer;
  xLinearObject: TLinearObject;
begin
  xIndex := ComboEditSymbol.ItemIndex;
  xItem := ComboEditSymbol.Text;
  if (not xItem.IsEmpty) and (xIndex >= 0) then
  begin
    TSetting.WriteString('config.sys.Symbol',xItem);
    xLinearObject := InstrumentsInfo.LinearObjects[xIndex];

    InstrumentInfo.AsString['Symbol'] := xLinearObject.Symbol;
    InstrumentInfo.AsString['PriceFilter.TickSize'] := xLinearObject.PriceFilter.TickSize;
    InstrumentInfo.AsString['LotSizeFilter.QtyStep'] := xLinearObject.LotSizeFilter.QtyStep;
    InstrumentInfo.Save('instrument');
  end;
end;

procedure TMainForm.ButtonInstrumentsInfoClick(Sender: TObject);
begin
  Self.SelectedInstrumentsInfo;
end;


procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  if Timer.Enabled then
    SetStop
  else
    SetStart;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  // Обновление индикатора
  HistoryIndicator.UpDate;
  if not IndicatorFrame.SetUpDate(HistoryIndicator) then
    SetStop;
end;

end.
