unit Lb.ModuleTableFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Layouts,
  Lb.Create.DB,
  FMX.ListBox,
  FMX.Menus;

type
  ///<summary>Список модулей</summary>
  TModuleTableFrame = class(TFrame)
    Layout: TLayout;
    ListBox: TListBox;
    PopupMenu: TPopupMenu;
    MenuItemAdd: TMenuItem;
    MenuItemChange: TMenuItem;
    MenuItemDelete: TMenuItem;
    procedure MenuItemAddClick(Sender: TObject);
    procedure MenuItemChangeClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
  private
    FModules: TCrModules;
    procedure SetModules(const Value: TCrModules);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Modules: TCrModules read FModules write SetModules;
  end;

implementation

{$R *.fmx}

uses
  Lb.Core.Events,
  Lb.SysUtils;

{ TModulesFrame }

constructor TModuleTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModules := nil;
end;

destructor TModuleTableFrame.Destroy;
begin

  inherited;
end;

procedure TModuleTableFrame.SetModules(const Value: TCrModules);
begin
  FModules := Value;
end;

procedure TModuleTableFrame.MenuItemAddClick(Sender: TObject);
begin
  ApplicationEvents.SetEvent(EVENT_MODULE_ADD,Self);
end;

procedure TModuleTableFrame.MenuItemChangeClick(Sender: TObject);
begin
  ApplicationEvents.SetEvent(EVENT_MODULE_CHANGE,Self);
end;

procedure TModuleTableFrame.MenuItemDeleteClick(Sender: TObject);
begin
  ApplicationEvents.SetEvent(EVENT_MODULE_DELETE,Self);
end;

end.
