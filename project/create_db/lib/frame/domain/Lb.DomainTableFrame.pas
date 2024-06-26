﻿unit Lb.DomainTableFrame;

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
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Controls.Presentation,
  FMX.Layouts,
  Lb.Create.DB,
  FMX.Objects;

type
  TDomainTableFrame = class(TFrame)
    Layout: TLayout;
    LayoutMenu: TLayout;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    StringGrid: TStringGrid;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
  private
    FDomains: TCrDomains;
    procedure SetDomains(const Value: TCrDomains);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Domains: TCrDomains read FDomains write SetDomains;
  end;

implementation

{$R *.fmx}

uses
  Lb.Core.Events,
  Lb.SysUtils;

{ TDomainTableFrame }

constructor TDomainTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TDomainTableFrame.Destroy;
begin

  inherited Destroy;
end;

procedure TDomainTableFrame.SetDomains(const Value: TCrDomains);
begin
  FDomains := Value;
end;

procedure TDomainTableFrame.ButtonAddClick(Sender: TObject);
begin
  ApplicationEvents.SetEvent(EVENT_DOMAIN_TABLE_ADD,Self);
end;

procedure TDomainTableFrame.ButtonEditClick(Sender: TObject);
begin
  ApplicationEvents.SetEvent(EVENT_DOMAIN_TABLE_CHANGE,Self);
end;

procedure TDomainTableFrame.ButtonDeleteClick(Sender: TObject);
begin
  ApplicationEvents.SetEvent(EVENT_DOMAIN_TABLE_DELETED,Self);
end;

end.
