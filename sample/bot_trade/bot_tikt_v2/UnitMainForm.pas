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
  Lb.SysUtils.Candel, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  UnitBlocksFrame,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid,
  Lb.Trades;

type
  TFormMain = class(TForm)
    ButtonStartAnsStop: TButton;
    Timer: TTimer;
    StrGrid: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    TextTiket: TText;
    LayoutClient: TLayout;
    LayoutMenu: TLayout;
    TextTrade: TText;
    GridPanelLayout: TGridPanelLayout;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    Splitter: TSplitter;
    StringColumn9: TStringColumn;
    procedure ButtonStartAnsStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure SetTitleButtom;
  public
    MemoryTikets: TMemoryTikets;
    Blocks: TBlocksFrame;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  UnitLineFrame;


const
  FILE_NAME_TIKE     = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\SPFB.SBRF-12.22_221111_221111.csv';


{ TFormMain }

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  MemoryTikets := TMemoryTikets.Create;

  Blocks := TBlocksFrame.Create(nil);
  Blocks.Parent := LayoutClient;
  Blocks.Align := TAlignLayout.Client;

end;

destructor TFormMain.Destroy;
begin
  FreeAndNil(Blocks);
  FreeAndNil(MemoryTikets);
  inherited;
end;


procedure TFormMain.SetTitleButtom;
begin
  if Timer.Enabled then
    ButtonStartAnsStop.Text := '����'
  else
    ButtonStartAnsStop.Text := '����';
end;

procedure TFormMain.ButtonStartAnsStopClick(Sender: TObject);
begin
  MemoryTikets.FileName := FILE_NAME_TIKE;
  Timer.Enabled := not Timer.Enabled;
  SetTitleButtom;
  MemoryTikets.First;
end;

procedure TFormMain.TimerTimer(Sender: TObject);

  procedure _SetUpGrid;
  var
    xSum: Double;
    xTrade: TTrade;
    i, iCount: Integer;
  begin
    xSum := 0;
    iCount := Blocks.Trades.Items.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xTrade := Blocks.Trades.Items[i];
        // ---------------------------------------------
        StrGrid.Cells[0,i] := xTrade.OpenPrice.ToString;
        StrGrid.Cells[1,i] := xTrade.Quantity.ToString;
        StrGrid.Cells[2,i] := xTrade.BuySell;
        StrGrid.Cells[3,i] := xTrade.ClosePrice.ToString;
        StrGrid.Cells[4,i] := xTrade.Profit.ToString;
        case xTrade.Status of
          stOpen: StrGrid.Cells[5,i] := 'open';
          stClose: StrGrid.Cells[5,i] := 'close';
        end;
        StrGrid.Cells[6,i] := xTrade.MaxProfit.ToString;
        StrGrid.Cells[7,i] := xTrade.MinProfit.ToString;
        StrGrid.Cells[8,i] := xTrade.StopProfit.ToString;
        xSum := xSum + xTrade.Profit;
      end;
    TextTrade.Text := xSum.ToString;
  end;

var
  xTiket: TTiket;
begin
  try
    if MemoryTikets.EOF then
    begin
      Timer.Enabled := False;
      SetTitleButtom;
    end
    else
    begin
      xTiket := MemoryTikets.Tiket;
      Blocks.SetTikit(xTiket.Price,xTiket.Vol);
      MemoryTikets.Next;
    end;
    TextTiket.Text := '�����: ' + xTiket.ToString;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
      Timer.Enabled := False;
      SetTitleButtom;
    end;
  end;
  _SetUpGrid;
end;

end.