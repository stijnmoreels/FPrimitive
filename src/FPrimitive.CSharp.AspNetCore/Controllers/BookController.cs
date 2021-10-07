using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using FPrimitive.CSharp.AspNetCore.Model.Serialization;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;

namespace FPrimitive.CSharp.AspNetCore.Controllers
{
    [Route("api/book")]
    [ApiController]
    public class BookController : ControllerBase
    {
        [HttpPost]
        public IActionResult Post(BookJson book)
        {
            return Ok();
        }
    }
}
