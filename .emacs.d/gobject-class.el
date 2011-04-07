;;; gobject-class.el --- functions to easy GObject-based class developers
;; Author: Gustavo Sverzut Barbieri <barbieri@gmail.com>
;; Author: Youness Alaoui <youness.alaoui@collabora.co.uk>
;; Copyright: public domain
;; URL: http://blog.gustavobarbieri.com.br/old-website/gobject-class.el
;; EmacsWiki: GObjectClassHelpers
;; Keywords: gobject, glib, gtk, helper, utilities
;;; Commentary:
;;; Code:


(defun string-join (list separator)
  "Takes a list of string and joins them using delimiter."
  (mapconcat (lambda (x) x) list separator))

(defun string-concat (list)
  "Takes a list of strings and joins them."
  (mapconcat (lambda (x) x) list ""))

(defun ask-value-non-empty (prompt)
  "Ask a question in minibuffer and ensure it's not empty string."
  (let ((x (read-string prompt)))
    (if (string= "" x)
	(ask-value-non-empty prompt)
      x)))

(defun gobject-class-header (class_name parent_class_name private)
  "Generate the header definition for a GObject derived class.

It takes 2 parameters:
   CLASS_NAME: class name, like 'gtk_button' or 'gtk_tree_view'. First
      element before the underscore character (_) will be used as name
      space. Example: 'gtk_button' is the 'button' class in 'gtk' name
      space.
   PARENT_CLASS_NAME: parent class name, like 'g_object'. First element
      before the underscore character (_) will be used as name space.
      Example: 'g_object' is the 'object' class in 'g' namespace.
   PRIVATE: Whether or not to include a Private structure and priv field.
"
  (interactive "sClass name (ie: gtk_tree_view): \nsParent class name (default: g_object): \nsAdd Private structure ([y]/n)?\n")

  (defun right-fill (len string)
    "Takes a string and fill it to take at least len characters"
    (setq missing (- len (length string)))
    (if	(> missing 0)
	(concat string (make-string missing ?\  ))
      string))
  (defun macro-line-cnt (&rest sequences)
    (concat (right-fill 72 (string-concat sequences)) "\\\n"))
  (defun macro-line-end (&rest sequences)
    (concat (string-concat sequences) "\n"))


  (let* ((parent_class_name (if (string= "" parent_class_name)
				"g_object"
			      (downcase parent_class_name)))
	 (class_name        (downcase (if (string= "" class_name)
					  (ask-value-non-empty
					   "You must provide class name (ie: gtk_tree_view): ")
					class_name)))
	 (private (if (string= "n" private) "n" "y"))
	 (pieces-class_name  (split-string (downcase class_name) "_"))
	 (pieces-parent_class_name (split-string parent_class_name "_"))
	 (namespace       (car-safe pieces-class_name))
	 (name            (string-join (cdr-safe pieces-class_name) "_"))
	 (ParentClassName (mapconcat 'capitalize pieces-parent_class_name ""))
	 (NAMESPACE       (upcase namespace))
	 (NAME            (upcase name))
	 (NameSpace       (capitalize namespace))
	 (Name            (mapconcat 'capitalize (cdr-safe pieces-class_name) ""))
	 (ClassName       (concat NameSpace Name))
	 )

    (insert
     (concat
      "\n"
      "G_BEGIN_DECLS\n"
      "\n"
      (macro-line-cnt "#define " NAMESPACE "_TYPE_" NAME)
      (macro-line-end "   (" namespace "_" name "_get_type())")
      (macro-line-cnt "#define " NAMESPACE "_" NAME "(obj)")
      (macro-line-cnt "   (G_TYPE_CHECK_INSTANCE_CAST((obj),")
      (macro-line-cnt "                               " NAMESPACE "_TYPE_" NAME ",")
      (macro-line-end "                               " ClassName "))")
      (macro-line-cnt "#define " NAMESPACE "_" NAME "_CLASS(klass)")
      (macro-line-cnt "   (G_TYPE_CHECK_CLASS_CAST((klass),")
      (macro-line-cnt "                            " NAMESPACE "_TYPE_" NAME ",")
      (macro-line-end "                            " ClassName "Class))")
      (macro-line-cnt "#define " NAMESPACE "_IS_" NAME "(obj)")
      (macro-line-cnt "   (G_TYPE_CHECK_INSTANCE_TYPE((obj),")
      (macro-line-end "                              " NAMESPACE "_TYPE_" NAME "))")
      (macro-line-cnt "#define " NAMESPACE "_IS_" NAME "_CLASS(klass)")
      (macro-line-cnt "   (G_TYPE_CHECK_CLASS_TYPE((klass),")
      (macro-line-end "                            " NAMESPACE "_TYPE_" NAME "))")
      (macro-line-cnt "#define " NAMESPACE "_" NAME "_GET_CLASS(obj)")
      (macro-line-cnt "   (G_TYPE_INSTANCE_GET_CLASS((obj),")
      (macro-line-cnt "                              " NAMESPACE "_TYPE_" NAME ",")
      (macro-line-end "                              " ClassName "Class))")
      "\n"
      "typedef struct _" ClassName "      " ClassName ";\n"
      "typedef struct _" ClassName "Class " ClassName "Class;\n"
      (if (string= "y" private)
	  (concat "typedef struct _" ClassName "Private " ClassName "Private;\n")
	"")

      "\n"
      "struct _" ClassName "Class\n"
      "{\n"
      "    " ParentClassName "Class parent_class;\n"
      "};\n"
      "\n"
      "struct _" ClassName "\n"
      "{\n"
      "    " ParentClassName " parent;\n"
      (if (string= "y" private)
	  (concat "    " ClassName "Private *priv;\n")
	  "")
      "};\n"
      "\n"
      "GType " namespace "_" name "_get_type(void) G_GNUC_CONST;\n"
      "\n"
      "G_END_DECLS\n"
      "\n"
      )
     )
    )
  )

(defun gobject-class-code (class_name parent_class_name private properties signals)
  "Generate the code implementation for a GObject derived class.

It takes 2 parameters:
   CLASS_NAME: class name, like 'gtk_button' or 'gtk_tree_view'. First
      element before the underscore character (_) will be used as name
      space. Example: 'gtk_button' is the 'button' class in 'gtk' name
      space.
   PARENT_CLASS_NAME: parent class name, like 'g_object'. First element
      before the underscore character (_) will be used as name space.
      Example: 'g_object' is the 'object' class in 'g' namespace.
   PRIVATE: Whether or not to include a Private structure and priv field.
   PROPERTIES: Whether or not to include properties.
   SIGNALS: Whether or not to include signals.
"
  (interactive "sClass name (ie: gtk_tree_view): \nsParent class name (default: g_object): \nsAdd Private structure ([y]/n)?\nsAdd properties ([y]/n)?\nsAdd signals ([y]/n)?\n")

  (let* ((parent_class_name (if (string= "" parent_class_name)
				"g_object"
			      (downcase parent_class_name)))
	 (class_name        (downcase (if (string= "" class_name)
					  (ask-value-non-empty
					   "You must provide class name (ie: gtk_tree_view): ")
					class_name)))
	 (private (if (string= "n" private) "n" "y"))
	 (properties (if (string= "n" properties) "n" "y"))
	 (signals (if (string= "n" signals) "n" "y"))
	 (pieces-class_name  (split-string (downcase class_name) "_"))
	 (pieces-parent_class_name (split-string parent_class_name "_"))
	 (namespace       (car-safe pieces-class_name))
	 (name            (string-join (cdr-safe pieces-class_name) "_"))
	 (ParentClassName (mapconcat 'capitalize pieces-parent_class_name ""))
	 (NameSpace       (capitalize namespace))
	 (Name            (mapconcat 'capitalize (cdr-safe pieces-class_name) ""))
	 (ClassName       (concat NameSpace Name))
	 (class_name      (concat namespace "_" name))
	 (NAMESPACE       (upcase namespace))
	 (NAME            (upcase name))
         (PARENT_NAMESPACE (upcase (car-safe pieces-parent_class_name)))
         (PARENT_NAME      (mapconcat 'upcase (cdr-safe pieces-parent_class_name) "_"))
	 )

    (insert
     (concat
      "\n"
      "G_DEFINE_TYPE(" ClassName ", " class_name ", " PARENT_NAMESPACE "_TYPE_" PARENT_NAME ");\n"
      "\n"
      "static void " class_name "_dispose(GObject *object);\n"
      "static void " class_name "_finalize(GObject *object);\n"
      (if (string= "y" properties)
	  (concat "static void " class_name "_get_property(GObject *object,\n"
		  "   guint property_id, GValue *value, GParamSpec *pspec);\n"
		  "static void " class_name "_set_property(GObject *object,\n"
		  "   guint property_id, const GValue *value, GParamSpec *pspec);\n")
	"")
      "\n"
      (if (string= "y" signals)
	  (concat   "/* signal enum */\n"
		    "enum {\n"
		    "    SIGNAL_DUMMY,\n"
		    "    LAST_SIGNAL\n"
		    "};\n"
		    "\n"
		    "static guint signals[LAST_SIGNAL] = {0};\n"
		    "\n")
	"")

      (if (string= "y" properties)
	  (concat      "/* properties */\n"
		       "enum {\n"
		       "  PROP_DUMMY = 1,\n"
		       "  LAST_PROPERTY\n"
		       "};\n")
	"")
      "\n"
      (if (string= "y" private)
	  (concat    "struct _" ClassName "Private\n"
		     "{\n"
		     "  guint dummy;\n"
		     "};\n"
		     "\n")
	"")
      "static void\n"
      class_name "_class_init(" ClassName "Class *klass)\n"
      "{\n"
      "    GObjectClass *gobject_class = G_OBJECT_CLASS(klass);\n"
      "\n"
      (if (string= "y" private)
	  (concat  "    g_type_class_add_private(klass, sizeof(" ClassName "Private));\n"
		   "\n")
	"")
      (if (string= "y" properties)
	  (concat       "    gobject_class->get_property = " class_name "_get_property;\n"
			"    gobject_class->set_property = " class_name "_set_property;\n")
	"")
      "    gobject_class->dispose = " class_name "_dispose;\n"
      "    gobject_class->finalize = " class_name "_finalize;\n"
      "\n"
      (if (string= "y" properties)
	  (concat   "    g_object_class_install_property(gobject_class, PROP_DUMMY,\n"
		    "        g_param_spec_uint(\"dummy\", \"dummy property\",\n"
		    "            \"dummy property blurp.\",\n"
		    "            0, G_MAXUINT,\n"
		    "            0,\n"
		    "            G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));\n"
		    "\n")
	"")
      (if (string= "y" properties)
	  (concat  "    signals[SIGNAL_DUMMY] = g_signal_new(\"dummy\",\n"
		   "        G_OBJECT_CLASS_TYPE(klass),\n"
		   "        G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,\n"
		   "        0,\n"
		   "        NULL, NULL,\n"
		   "        g_cclosure_marshal_VOID__VOID,\n"
		   "        G_TYPE_NONE, 0);\n"
		   "\n")
	"")
      "}\n"
      "\n"
      "static void\n"
      class_name "_init(" ClassName " *self)\n"
      "{\n"
      (if (string= "y" private)
	  (concat  "    " ClassName "Private *priv =\n"
		   "        G_TYPE_INSTANCE_GET_PRIVATE(self, " NAMESPACE "_TYPE_" NAME ",\n"
		   "            " ClassName "Private);\n"
		   "\n"
		   "    self->priv = priv;\n")
	"")
      "}\n"
      "\n"
      (if (string= "y" properties)
	  (concat     "static void\n"
		      class_name "_get_property(GObject *object,\n"
		      "  guint property_id, GValue *value, GParamSpec *pspec)\n"
		      "{\n"
		      "    " ClassName" *self = " NAMESPACE "_" NAME "(object);\n"
		      (if (string= "y" private)
			  (concat   "    " ClassName "Private *priv = self->priv;\n"
				    "\n"
				    "    /* Make compiler happy */\n"
				    "    (void)priv;\n")
			"")
		      "\n"
		      "    switch (property_id) {\n"
		      "      case PROP_DUMMY:\n"
		      "        g_value_set_uint(value, 0);\n"
		      "        break;\n"
		      "      default:\n"
		      "        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);\n"
		      "        break;\n"
		      "    }\n"
		      "}\n"
		      "\n"
		      "static void\n"
		      class_name "_set_property(GObject *object,\n"
		      "  guint property_id, const GValue *value, GParamSpec *pspec)\n"
		      "{\n"
		      "    " ClassName" *self = " NAMESPACE "_" NAME "(object);\n"
		      (if (string= "y" private)
			  (concat   "    " ClassName "Private *priv = self->priv;\n"
				    "\n"
				    "    /* Make compiler happy */\n"
				    "    (void)priv;\n")
			"")
		      "\n"
		      "    switch (property_id) {\n"
		      "      case PROP_DUMMY:\n"
		      "        g_value_get_uint(value);\n"
		      "        break;\n"
		      "      default:\n"
		      "        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);\n"
		      "        break;\n"
		      "    }\n"
		      "}\n")
	"")
      "\n"
      "\n"
      "static void\n"
      class_name "_dispose(GObject *object)\n"
      "{\n"
      "    " ClassName " *self = (" ClassName " *)object;\n"
      (if (string= "y" private)
	  (concat   "    " ClassName "Private *priv = self->priv;\n")
	"")
      "\n"
      "    G_OBJECT_CLASS(" class_name "_parent_class)->dispose(object);\n"
      "}\n"
      "\n"
      "static void\n"
      class_name "_finalize(GObject *object)\n"
      "{\n"
      "    " ClassName " *self = (" ClassName " *)object;\n"
      "\n"
      "    /* Make compiler happy */\n"
      "    (void)self;\n"
      "\n"
      "    G_OBJECT_CLASS(" class_name "_parent_class)->finalize(object);\n"
      "}\n"
      "\n"
      )
     )
    )
  )

(defun gobject-class-generate (package class_name parent_class_name copyright private properties signals)
  "Generate header (.h) and code (.c) files for a GObject derived class.

It takes 2 parameters:
   CLASS_NAME: class name, like 'gtk_button' or 'gtk_tree_view'. First
      element before the underscore character (_) will be used as name
      space. Example: 'gtk_button' is the 'button' class in 'gtk' name
      space.
   PARENT_CLASS_NAME: parent class name, like 'g_object'. First element
      before the underscore character (_) will be used as name space.
      Example: 'g_object' is the 'object' class in 'g' namespace.
"
  (interactive "sPackage name: \nsClass name (ie: gtk_tree_view): \nsParent class name (default: g_object): \nsCopyright information : \nsAdd Private structure ([y]/n)?\nsAdd properties ([y]/n)?\nsAdd signals ([y]/n)?\n")

  (let* ((parent_class_name (if (string= "" parent_class_name)
				"g_object"
			      (downcase parent_class_name)))
	 (class_name        (downcase (if (string= "" class_name)
					  (ask-value-non-empty
					   "You must provide class name (ie: gtk_tree_view): ")
					class_name)))
	 (private (if (string= "n" private) "n" "y"))
	 (properties (if (string= "n" properties) "n" "y"))
	 (signals (if (string= "n" signals) "n" "y"))
	 (copyright_line (if (string= "" copyright)
			     ""
			   (concat " * Copyright (C) " (number-to-string (nth 5 (decode-time (current-time)))) " " copyright "\n\n")))
	 (pieces-class_name  (split-string (downcase class_name) "_"))
	 (pieces-parent_class_name (split-string parent_class_name "_"))
	 (namespace       (car-safe pieces-class_name))
	 (name            (string-join (cdr-safe pieces-class_name) "_"))
	 (NameSpace       (capitalize namespace))
	 (Name            (mapconcat 'capitalize (cdr-safe pieces-class_name) ""))
	 (ClassName       (concat NameSpace Name))
	 (base_file_name (string-join pieces-class_name "-"))
	 (file_header (concat base_file_name ".h"))
	 (file_code   (concat base_file_name ".c"))
	 (DEFINE_NAME (concat (upcase class_name) "_H"))
	 (parent_include (if (string= "g_object" parent_class_name)
			     "glib-object.h"
			   (if (string-match "^gtk_" parent_class_name)
			       "gtk/gtk.h"
			     (concat (string-join pieces-parent_class_name "-") ".h")
			     )
			   ))
	 )

    (delete-other-windows)
    (split-window-vertically)

    (find-file file_header)
    (insert
     (concat
      "/*\n"
      copyright_line
      " *\n"
      " * " package " is free software: you can redistribute it and/or modify\n"
      " * it under the terms of the GNU General Public License as published by\n"
      " * the Free Software Foundation, either version 3 of the License, or\n"
      " * (at your option) any later version.\n"
      " *\n"
      " * " package " is distributed in the hope that it will be useful,\n"
      " * but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
      " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
      " * GNU General Public License for more details.\n"
      " *\n"
      " * You should have received a copy of the GNU General Public License\n"
      " * along with " package ".  If not, see <http://www.gnu.org/licenses/>.\n"
      " */\n"
      "\n"
      "#ifndef __" DEFINE_NAME "__\n"
      "#define __" DEFINE_NAME "__\n"
      "\n"
      "#include <" parent_include ">\n"
      "\n"))
    (gobject-class-header class_name parent_class_name private)
    (insert "#endif /* __" DEFINE_NAME "__ */\n")

    (indent-region (point-min) (point-max))

    (other-window 1)

    (find-file file_code)
    (insert
     (concat
      "/*\n"
      copyright_line
      " *\n"
      " * " package " is free software: you can redistribute it and/or modify\n"
      " * it under the terms of the GNU General Public License as published by\n"
      " * the Free Software Foundation, either version 3 of the License, or\n"
      " * (at your option) any later version.\n"
      " *\n"
      " * " package " is distributed in the hope that it will be useful,\n"
      " * but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
      " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
      " * GNU General Public License for more details.\n"
      " *\n"
      " * You should have received a copy of the GNU General Public License\n"
      " * along with " package ".  If not, see <http://www.gnu.org/licenses/>.\n"
      " */\n"
      "\n"
      "#include \"" file_header "\"\n"
      "\n"))
    (gobject-class-code class_name parent_class_name private properties signals)
    (indent-region (point-min) (point-max))
    )
  )

(provide 'gobject-class)

;;; gobject-class.el ends here
