unit TMBEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
    TMBEditor = Class
      private
             FEditorName : String;
             FEditorSurname : String
             FEditorID : Integer;
             FNewEditor : Boolean;
             FEditorBooks : array of Integer;
             function GetEditorBookID(Index : Integer) : Integer;
             procedure GetEditorBooks(const SQLQuery :  TSQLQuery);
      published

      public
             constructor Create(Name : String = ""; Surname : String = "");
             constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
             property EditorName : String read FEditorName write SetEditorName;
             property EditorSurname : String read FEditorSurname write SetEditorSurname;
             property EditorID : String read FEditorID;
             property EditorBooks[Index : Integer]: FEditorBooks read GetEditorBookID;
             procedure UpdateEditor(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);

    end;

implementation

end.

