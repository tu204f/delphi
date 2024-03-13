unit UnitMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.ExtCtrls,
  Quik.Manager.DDE,
  Quik.SysUtils,
  Quik.ValueTable;

type
  ///<summary>Главная форма</summary>
  TMainForm = class(TForm)
    ButtonTolls: TButton;
    StrGrid: TStringGrid;
    EditSecurity: TEdit;
    Button1: TButton;
    Button2: TButton;
    ButtonQuikTable: TButton;
    Timer: TTimer;
    procedure ButtonTollsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuikTableClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure SetShowOrders;
  public
    QuikTable: TQuikTable;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  UnitQuikTableForm,
  UnitToolsForm,
  Lb.Setting,
  UnitAddOrderForm,
  Lb.SysUtils;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  QuikTable := nil;
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Self.Caption := 'Управление заявками (По RSI)';
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _SetConnetQuikTable;
  var
    xQuikTableName: String;
  begin
    xQuikTableName := TSetting.ReadString('config.sys.quik_table_rsi','');
    if not xQuikTableName.IsEmpty then
    begin
      var xInd := QuikManagerTable.IndexOfTable(xQuikTableName);
      if xInd >= 0 then
        QuikTable := QuikManagerTable.Tables[xInd];
    end;
  end;


  procedure _EventOrder;
  begin

  end;

var
  xS: String;
begin
  // отслеживаем
  if Assigned(QuikTable) then
  begin
    QuikTable.Fisrt;
    xS := 'QUIK.RSI:' + QuikTable.AsByIndexString(8);
    EditSecurity.Text := xS;

    SetShowOrders;
    _EventOrder;


  end else
    _SetConnetQuikTable;
end;

procedure TMainForm.ButtonTollsClick(Sender: TObject);
begin
  // Настройка работы
  ToolsForm.ShowModal;
end;

procedure TMainForm.ButtonQuikTableClick(Sender: TObject);
begin
  QuikTableForm.Show;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if AddOrderForm.ShowModal = mrOk then
  begin
    GetUserOrders.Add(AddOrderForm.UserOrder);
    SetShowOrders;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  xRow: Integer;
begin
  xRow := StrGrid.Row - 1;
  if (xRow >= 0) and (xRow < GetUserOrders.Count) then
  begin
    GetUserOrders.Delete(xRow);
    SetShowOrders;
  end;
end;

procedure TMainForm.SetShowOrders;

  procedure _Header;
  begin
    with StrGrid.Rows[0] do
    begin
      Clear;
      Add('SecCode');
      Add('CodeClass');
      Add('Quantity');
      Add('RSI');
      Add('StepPrice');
      Add('BuySell');
      Add('MktLmt');
      Add('Direction');
    end;
  end;

var
  xOrder: TUserOrder;
  i, iCount: Integer;
begin
  StrGrid.ColCount := 8;
  _Header;
  iCount := GetUserOrders.Count;
  if iCount > 0 then
  begin
    StrGrid.RowCount := iCount + 1;
    for i := 0 to iCount - 1 do
    begin
      xOrder := GetUserOrders.Items[i];
      with StrGrid.Rows[i + 1] do
      begin
        Clear;
        Add(xOrder.SecCode);
        Add(xOrder.CodeClass);
        Add(xOrder.Quantity.ToString);
        Add(xOrder.ValueRSI.ToString);
        Add(xOrder.StepPrice.ToString);
        Add(GetStrToBuySell(xOrder.BuySell));
        Add(GetStrToMktLmt(xOrder.MktLmt));
        Add(GetStrToDirection(xOrder.Direction));
      end;
    end;
  end
  else
  begin
    StrGrid.RowCount := 2;
    StrGrid.Rows[1].Clear;
  end;
end;

end.
