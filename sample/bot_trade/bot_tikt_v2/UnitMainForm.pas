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
  FMX.ListBox, FMX.Objects,
  UnitLineFrame,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, System.Rtti,
  FMX.Grid.Style, FMX.Grid;

type
  TFormMain = class(TForm)
    ButtonStartAnsStop: TButton;
    Timer: TTimer;
    Layout1: TLayout;
    StrGrid: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    Text1: TText;
    procedure ButtonStartAnsStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure SetTitleButtom;
    procedure EventChangeBlock(Sender: TObject; APrice: Double; ATypeBlock: TTypeBlock);
  public
    MemoryTikets: TMemoryTikets;
    LineFrame: TLineFrame;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

const
  FILE_NAME_TIKE     = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\SPFB.SBRF-12.22_221111_221111.csv';


{ TFormMain }

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  MemoryTikets := TMemoryTikets.Create;

  LineFrame := TLineFrame.Create(nil);
  LineFrame.Parent := Layout1;
  LineFrame.Align := TAlignLayout.Client;
  LineFrame.LineTikets.OnChangeBlock := EventChangeBlock;
end;

destructor TFormMain.Destroy;
begin
  FreeAndNil(LineFrame);
  FreeAndNil(MemoryTikets);
  inherited;
end;


procedure TFormMain.SetTitleButtom;
begin
  if Timer.Enabled then
    ButtonStartAnsStop.Text := 'Стоп'
  else
    ButtonStartAnsStop.Text := 'Стоп';
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
    iCount := LineFrame.LineTikets.Trades.Items.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xTrade := LineFrame.LineTikets.Trades.Items[i];
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
        xSum := xSum + xTrade.Profit;
      end;
    Text1.Text := xSum.ToString;
  end;

var
  xTiket: TTiket;
begin
  if MemoryTikets.EOF then
  begin
    Timer.Enabled := False;
    SetTitleButtom;
  end
  else
  begin
    xTiket := MemoryTikets.Tiket;
    LineFrame.AddTiket(xTiket.Price,xTiket.Vol);
    MemoryTikets.Next;
  end;

  _SetUpGrid;
end;

procedure TFormMain.EventChangeBlock(Sender: TObject; APrice: Double; ATypeBlock: TTypeBlock);
begin
  //
end;

end.
