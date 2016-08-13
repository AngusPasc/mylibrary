unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, DbCtrls, Grids;

type

  { TMyBooks }

  TMyBooks = class(TForm)
    btnBooks: TButton;
    btnAuthors: TButton;
    btnGenres: TButton;
    dtnAddBook: TButton;
    DataSource1: TDataSource;
    edtBookName: TEdit;
    edtBookISBN: TEdit;
    edtOrigName: TEdit;
    edtBookYear: TEdit;
    edtBookAnnotation: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StringGrid1: TStringGrid;
    procedure btnAuthorsClick(Sender: TObject);
    procedure btnGenresClick(Sender: TObject);
    procedure btnBooksClick(Sender: TObject);
    procedure dtnAddBookClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MyBooks: TMyBooks;

implementation

{$R *.lfm}

{ TMyBooks }

procedure TMyBooks.FormCreate(Sender: TObject);
begin
  //SQLQuery1.Close;
end;

procedure TMyBooks.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     SQLQuery1.Close;
     SQLTransaction1.Active:=False;
     SQLite3Connection1.Connected:=False;
end;

procedure TMyBooks.btnBooksClick(Sender: TObject);
var
  I : Integer;
  c: TGridColumn;
begin
     //DBGrid1.Columns;
     SQLQuery1.Close;
     SQLQuery1.SQL.Text:='SELECT name, isbn, orig_name, year, note FROM books';
     SQLQuery1.Open;
     ShowMessage(SQLQuery1.FieldByName('name').AsString);
     //SQLTransaction1.Commit;
     for I:= 0 to SQLQuery1.Fields.Count - 1 do
     begin
          c := StringGrid1.Columns.Add;
          c.Title.Caption:=SQLQuery1.Fields[i].FieldName;

     end;
end;

procedure TMyBooks.dtnAddBookClick(Sender: TObject);
begin
     SQLQuery1.Close;
     SQLQuery1.SQL.Text:='INSERT INTO books (name, isbn, orig_name, year, note) VALUES (:bNAME, :bISBN, :bORIGNAME, :bYEAR, :bNOTE)';
     SQLQuery1.Params.ParamByName('bNAME').AsString:=edtBookName.Text;
     SQLQuery1.Params.ParamByName('bISBN').AsString:=edtBookISBN.Text;
     SQLQuery1.Params.ParamByName('bORIGNAME').AsString:=edtOrigName.Text;
     SQLQuery1.Params.ParamByName('bYEAR').AsString:=edtBookYear.Text;
     SQLQuery1.Params.ParamByName('bNOTE').AsString:=edtBookAnnotation.Text;
     SQLQuery1.ExecSQL;
     SQLTransaction1.Commit;
end;

procedure TMyBooks.btnGenresClick(Sender: TObject);
begin

end;

procedure TMyBooks.btnAuthorsClick(Sender: TObject);
begin

end;

end.

