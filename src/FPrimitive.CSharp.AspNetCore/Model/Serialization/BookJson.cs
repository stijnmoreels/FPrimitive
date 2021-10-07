using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace FPrimitive.CSharp.AspNetCore.Model.Serialization
{
    public class BookJson
    {
        [RegularExpression("[a-z]+")]
        public string[] Authors { get; set; }

        [Required]
        public int Pages { get; set; }

        [Required]
        [StringLength(13)]
        [JsonProperty("ISBN13")]
        //[RegularExpression("^[0-9]{13}$")]
        [Escape]
        public string Isbn13 { get; set; }
        
        [Required]
        [Range(1, 4)]
        public int Rating { get; set; }
    }
}
