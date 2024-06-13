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
  Quik.ValueTable,
  UnitUserOrderFrame;

type
  ///<summary>Главная форма</summary>
  TMainForm = class(TForm)
    ButtonTolls: TButton;
    EditSecurity: TEdit;
    ButtonQuikTable: TButton;
    Timer: TTimer;
    GridPanel: TGridPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    UserOrderBuy: TUserOrderFrame;
    UserOrderSell: TUserOrderFrame;
    procedure ButtonTollsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuikTableClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    QuikTable: TQuikTable;
    Security : TQuikTable;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.Math,
  UnitQuikTableForm,
  UnitToolsForm,
  Lb.Setting,
  UnitAddOrderForm,
  Lb.SysUtils, Lb.Logger;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  QuikTable := nil;
  Security  := nil;
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Self.Caption := 'Управление заявками (По RSI)';
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UserOrderBuy.BuySell := 'B';
  UserOrderBuy.SetLoad;
  UserOrderSell.BuySell := 'S';
  UserOrderSell.SetLoad;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  UserOrderBuy.SetSave;
  UserOrderSell.SetSave;
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

      xInd := QuikManagerTable.IndexOfTable('security');
      if xInd >= 0 then
      begin
        Security := QuikManagerTable.Tables[xInd];
        UserOrderBuy.Security := Security;
        UserOrderSell.Security := Security;
      end;
    end;
  end;

var
  xS: String;
  xRSI: Double;
begin
  try
    if Assigned(QuikTable) then
    begin
      QuikTable.Fisrt;
      xRSI := QuikTable.AsByIndexDouble(8);
      xS := RoundTo(xRSI,-1).ToString;
      EditSecurity.Text := xS;

      UserOrderBuy.SetValueRSI(xRSI);
      UserOrderSell.SetValueRSI(xRSI);

    end
    else
      _SetConnetQuikTable;
  except
    on E : Exception do
      TLogger.Log(E.ClassName + ' ошибка с сообщением : ' + E.Message)
  end;
end;

procedure TMainForm.ButtonTollsClick(Sender: TObject);
begin
  // Настройка работы
  Self.FormStyle := fsNormal;
  ToolsForm.ShowModal;
  Self.FormStyle := fsStayOnTop;
end;

procedure TMainForm.ButtonQuikTableClick(Sender: TObject);
begin
  QuikTableForm.Show;
end;





end.
